{-# OPTIONS_GHC -fno-full-laziness #-}
{-# LANGUAGE DataKinds, TypeOperators, FlexibleInstances, DeriveGeneric,
             TypeFamilies, FlexibleContexts #-}

module Model (
        Review,
        Model,
        VocabLen,
        vocabLen,
        initialize,
        update,
        gradient,
        average,
        test,
        validate
) where

import Data.Binary
import Data.Foldable
import Data.Functor.Identity
import qualified Data.IntMap.Strict as M
import Data.Maybe (fromJust)
import Data.Proxy
import qualified Data.Vector.Unboxed as U
import GHC.Generics (Generic)
import GHC.TypeNats
import Numeric.Backprop
import Numeric.OneLiner
import Numeric.LinearAlgebra (randn, flatten, norm_2)
import qualified Numeric.LinearAlgebra.Static as C
import Numeric.LinearAlgebra.Static.Backprop

import Data

type EmbedLen = 100
type StateLen = 80
type VocabLen = 1000
-- type OutputLen = 1

embedLen :: Int
embedLen = fromIntegral $ natVal (Proxy :: Proxy EmbedLen)

stateLen :: Int
stateLen = fromIntegral $ natVal (Proxy :: Proxy StateLen)

vocabLen :: Int
vocabLen = fromIntegral $ natVal (Proxy :: Proxy VocabLen)

type family HKD f a where
        HKD Identity a = a
        HKD f a = f a

data FNetwork f = FNetwork { weightsInGate :: !(HKD f (L StateLen (EmbedLen + StateLen)))
                           , biasInGate :: !(HKD f (R StateLen))
                           , weightsOutGate :: !(HKD f (L StateLen (EmbedLen + StateLen)))
                           , biasOutGate :: !(HKD f (R StateLen))
                           , weightsForgetGate :: !(HKD f (L StateLen (EmbedLen + StateLen)))
                           , biasForgetGate :: !(HKD f (R StateLen))
                           , weightsCell :: !(HKD f (L StateLen (EmbedLen + StateLen)))
                           , biasCell :: !(HKD f (R StateLen))
                           , weightsOutput :: !(HKD f (R StateLen))
                           }
        deriving Generic

type Network = FNetwork Identity
type EmbedMap = M.IntMap (R EmbedLen)
type Model = (Network, EmbedMap)

instance Backprop Network

instance Binary Network

instance Num Network where
        (+) = gPlus
        (-) = gMinus
        (*) = gTimes
        negate = gNegate
        abs = gAbs
        signum = gSignum
        fromInteger = gFromInteger

instance Fractional Network where
        (/) = gDivide
        recip = gRecip
        fromRational = gFromRational

initialize :: Integral idx => [idx] -> IO Model
initialize wordList =
        do net <- FNetwork <$> (fromJust . C.create . scale <$> randn stateLen (embedLen + stateLen))
                           <*> (pure 1)
                           <*> (fromJust . C.create . scale <$> randn stateLen (embedLen + stateLen))
                           <*> (pure 1)
                           <*> (fromJust . C.create . scale <$> randn stateLen (embedLen + stateLen))
                           <*> (pure 1)
                           <*> (fromJust . C.create . scale <$> randn stateLen (embedLen + stateLen))
                           <*> (pure 1)
                           <*> (C.unrow . fromJust . C.create . scale <$> randn 1 stateLen)
           embed <- M.fromList <$> mapM (\idx -> (,) <$> pure (fromIntegral idx)
                                                     <*> (C.unrow . fromJust . C.create . scale <$> randn 1 embedLen)
                                        )
                                        wordList
           return (net, embed)
        where scale x = x * 0.2

run :: (Reifies s W, Integral idx)
    => BVar s Network
    -> BVar s EmbedMap
    -> [idx]
    -> BVar s (R StateLen)
    -> BVar s (R StateLen)
    -> BVar s Double
run (BV network) _ [] _ out = liftOp1 sigmoidOp $ weightsOutput network <.> out
run networkVar@(BV network) embedMap (i : is) memory out =
        let wordVec = flip liftOp1 embedMap . op1 $
                    \map -> case M.lookup (fromIntegral i) map of
                                 Just v -> (v, M.singleton (fromIntegral i))
                                 Nothing -> (0, const M.empty)
            input = wordVec # out
            forgetGate = liftOp1 sigmoidOp $ weightsForgetGate network #> input + biasForgetGate network
            inGate = liftOp1 sigmoidOp $ weightsInGate network #> input + biasInGate network
            outGate = liftOp1 sigmoidOp $ weightsOutGate network #> input + biasOutGate network
            memory' = forgetGate * memory + inGate * liftOp1 tanhOp (weightsCell network #> input + biasCell network)
            out' = outGate * liftOp1 tanhOp memory'
        in run networkVar embedMap is memory' out'

test :: (Foldable f, Integral idx) => Model -> f idx -> Double
test (net, map) words = evalBP (run (constVar net) (constVar map) (toList words) 0) 0

err :: Reifies s W => BVar s Double -> Double -> BVar s Double
err res targ = -(btarg * log res + (1 - btarg) * log (1 - res))
        where btarg = constVar targ

gradient :: Foldable f => f Review -> Int -> Maybe Model -> Model -> Model
gradient samples count mvelocity (net, map) = velocity $
        gradBP2 (\net@(BV bnet) map ->
                        let sampleError (Review words target) =
                                   err (run net map (U.toList words) (constVar 0) (constVar 0)) target
                            bcount = constVar $ fromIntegral count
                        in foldr' (\x err -> err + sampleError x) 0 samples / bcount +
                           (foldr' (\m err -> err + sumElements (mmap' (** 2) m)) 0
                                   [weightsInGate bnet, weightsOutGate bnet, weightsForgetGate bnet, weightsCell bnet] +
                            sumElements (vmap' (** 2) $ weightsOutput bnet)
                           ) * constVar regFac

                ) net map
        where (vnet, vmap) | Just v <- mvelocity = v
                           | otherwise = (0, M.empty)
              velocity (gnet, gmap) = ( 0.9 * vnet + 0.1 * gnet
                                      , M.unionWith (\v g -> 0.9 * v + 0.1 * g) vmap gmap
                                      )
              weightsCount = fromIntegral $ 4 * stateLen * (stateLen + embedLen) + stateLen
              lambda = 0.5
              regFac = lambda / (2 * weightsCount)

update :: Model -> Model -> Model
update (gnet, gmap) (net, map) =
        (net - 0.1 * gnet, M.unionWith (\x dx -> x - 0.1 * dx) map gmap)

average :: Model -> Model -> Model
average (net1, map1) (net2, map2) =
        ((net1 + net2) / 2, M.unionWith (\x1 x2 -> (x1 + x2) / 2) map1 map2)

validate :: Foldable f => f Review -> Int -> Model -> Double
validate reviews count (net, map) = (/ fromIntegral count) . fromIntegral $
        foldr' (\(Review words score) -> (+ (round . abs $ test (net, map) (U.toList words) - score)))
               0
               reviews


sigmoid :: Floating a => a -> a
sigmoid x = 1 / (1 + exp (-x))

sigmoidOp :: Floating a => Op '[a] a
sigmoidOp = op1 $ \x -> let s = sigmoid x in (s, \g -> s * (1 - s) * g)

tanhOp :: Floating a => Op '[a] a
tanhOp = op1 $ \x -> let t = tanh x in (t, \g -> (1 - t * t) * g)
