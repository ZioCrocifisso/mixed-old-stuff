{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Data where

import Control.Applicative
import Data.Binary (Binary, encodeFile, decodeFile)
import Data.Char
import Data.Csv
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.HashTable.IO as H
import Data.IORef
import qualified Data.IntMap as M
import Data.List (sortOn)
import qualified Data.Vector.Binary
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Debug.Trace
import Data.Word (Word16)
import GHC.Generics
import System.Random

data Type = Train | Test deriving (Eq, Show)
data Sentiment = Positive | Negative | Unlabeled deriving (Eq, Show)

instance FromField Type where
    parseField s | s == "train" = pure Train
                 | s == "test" = pure Test
                 | otherwise = empty

instance FromField Sentiment where
    parseField s | s == "pos" = pure Positive
                 | s == "neg" = pure Negative
                 | s == "unsup" = pure Unlabeled
                 | otherwise = empty

data ReviewRecord = ReviewRecord !Int !Type !B.ByteString !Sentiment !B.ByteString
        deriving Generic

instance FromRecord ReviewRecord

data Review = Review !(U.Vector Word16) Double deriving (Show, Read, Generic)

type Dictionary = H.LinearHashTable B.ByteString Word16

instance Binary Review

recordToReview :: H.LinearHashTable B.ByteString (Word16, Int)
               -> IORef Word16
               -> ReviewRecord
               -> IO Review
recordToReview wordTable nextIdxRef (ReviewRecord _ _ text sentiment _) =
        let words = take 100 . map (BC.filter (\c -> not (isPunctuation c) || c == '/')) . BC.words $ BC.map toLower text
            score = case sentiment of
                         Positive -> 1
                         Negative -> 0
                         _ -> error "Unlabeled review"
            wordToInt word = do nextIdx <- readIORef nextIdxRef
                                mword <- H.lookup wordTable word
                                int <- case mword of
                                            Just (int, occur) ->
                                                    do H.insert wordTable word (int, occur + 1)
                                                       return int
                                            Nothing -> do H.insert wordTable word (nextIdx, 0)
                                                          writeIORef nextIdxRef $ nextIdx + 1
                                                          return nextIdx
                                return int
        in do intWords <- U.fromList <$> mapM wordToInt words
              return $ Review intWords score

loadDictionary :: FilePath -> IO Dictionary
loadDictionary path = decodeFile path >>= H.fromList

saveDictionary :: FilePath -> Dictionary -> IO ()
saveDictionary path dict = H.toList dict >>= encodeFile path

enumDictionary :: Dictionary -> IO [Word16]
enumDictionary = fmap (map snd) . H.toList

dictToWordTable :: Dictionary -> IO (H.LinearHashTable B.ByteString (Word16, Int), IORef Word16)
dictToWordTable dict = do wordList <- H.toList dict
                          nextIdxRef <- newIORef . (+ 1) . maximum . map (\(w, i) -> i) $ wordList
                          wordTable <- H.fromList $ map (\(w, i) -> (w, (i, 0))) wordList
                          return (wordTable, nextIdxRef)

reviewEncoder :: Dictionary -> IO (String -> IO (U.Vector Word16))
reviewEncoder dict = do (wordTable, nextIdxRef) <- dictToWordTable dict
                        return $ \s -> do let record = ReviewRecord 0 Train (BC.pack s) Positive BC.empty
                                          Review words _ <- recordToReview wordTable nextIdxRef record
                                          return words

loadReviews :: FilePath
            -> Either Int Dictionary
            -> IO ((V.Vector Review, V.Vector Review), Dictionary)
loadReviews path dictOrSize =
        do csv <- B.readFile path
           (wordTable, nextIdxRef) <- case dictOrSize of
                                           Left _ -> (,) <$> H.new <*> newIORef 0
                                           Right dict -> dictToWordTable dict
           case decode HasHeader csv of
                Left err -> error err
                Right records -> let labeledRecords = V.filter labeled records
                                     (trainRecords, testRecords) = V.partition train labeledRecords
                                     mostFrequent n = map (\(w, (i, _)) -> (w, i)) . take n . sortOn (negate . snd . snd)
                                     dict = case dictOrSize of
                                                 Left size -> H.toList wordTable >>= H.fromList . mostFrequent size
                                                 Right d -> return d
                                 in (,) <$> ((,) <$> mapM (recordToReview wordTable nextIdxRef) trainRecords
                                                 <*> mapM (recordToReview wordTable nextIdxRef) testRecords)
                                        <*> dict
           
        where labeled (ReviewRecord _ _ _ Unlabeled _) = False
              labeled _ = True
              train (ReviewRecord _ Train _ _ _) = True
              train (ReviewRecord _ Test _ _ _) = False
              
