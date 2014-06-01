module MIO where

import Control.Arrow (second)
import Control.Applicative
import Control.Monad hiding (join)

data RealWorld = RealWorld { input :: [String], output :: [String] }

newtype MIO a = MIO { unMIO :: RealWorld -> (RealWorld, a) }

instance Functor MIO where
	fmap f x = MIO $ second f . unMIO x

instance Applicative MIO where
	pure x = MIO $ flip (,) x

--	ff <*> x = MIO $ (\(rw, (f, x)) -> (rw, f x)) . (\(rw, f) -> unMIO (fmap ((,) f) x) rw) . unMIO ff
	(<*>) = ap

instance Monad MIO where
	return = pure

	x >>= f = join $ fmap f x
		where join m = MIO $ uncurry (flip unMIO) . unMIO m

mPrint :: Show a => a -> MIO ()
mPrint x = MIO $ \(RealWorld i o) -> (RealWorld i $ o ++ [ show x ], ())

mGet :: MIO String
mGet = MIO $ \(RealWorld (i:is) o) -> (RealWorld is o, i)

mCount :: MIO Int
mCount = MIO $ \(RealWorld i o) -> (RealWorld i o, length o)

mRemaining :: MIO Int
mRemaining = MIO $ \(RealWorld i o) -> (RealWorld i o, length i)

mPerform :: MIO () -> [String] -> RealWorld
mPerform as input = fst . unMIO as $ RealWorld input [""]

mExecute :: MIO () -> [String] -> IO ()
mExecute as = mapM_ putStrLn . output . mPerform as

