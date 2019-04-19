{-# OPTIONS_GHC -fno-full-laziness #-}

module Main where

import Control.Concurrent hiding (newChan)
import qualified Control.Distributed.Backend.P2P as P2P
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Exception.Base (evaluate)
import Control.Monad (join, forever)
import Data.Binary (encodeFile, decodeFile)
import Data.Maybe (isJust)
import Data.Vector.Binary
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.Directory (doesFileExist, copyFile)
import System.Environment (getArgs)
import System.IO
import System.Random
import VectorShuffling.Immutable

import Data
import Model

main :: IO ()
main = do args <- getArgs
          case args of
               ('d' : 'i' : 'c' : 't' : _) : _ -> initializeDictionary >> return ()
               ('i' : 'n' : 'i' : 't' : _) : _ -> initializeModel >> return ()
               ('t' : 'e' : 's' : 't' : _) : _ -> inputTest
               host : port : seeds -> train (host, port) seeds
               [] -> train ("", "") []
               _ -> error "Argomenti non validi"

initializeDictionary :: IO Dictionary
initializeDictionary = do (_, dict) <- loadReviews "imdb_master.csv" (Left Model.vocabLen)
                          saveDictionary "dictionary" dict
                          return dict

initializeModel :: IO Model
initializeModel = do (_, dict) <- initializeReviews
                     model <- enumDictionary dict >>= Model.initialize
                     encodeFile "model" model
                     return model

initializeReviews :: IO ((V.Vector Review, V.Vector Review), Dictionary)
initializeReviews = do dictExists <- doesFileExist "dictionary"
                       dictOrSize <- if dictExists
                                     then fmap Right $ loadDictionary "dictionary"
                                     else return $ Left Model.vocabLen
                       (sets, dict) <- loadReviews "imdb_master.csv" dictOrSize
                       encodeFile "reviews" sets
                       return (sets, dict)

inputTest :: IO ()
inputTest = do dictExists <- doesFileExist "dictionary"
               dict <- if dictExists
                       then loadDictionary "dictionary"
                       else fmap snd $ loadReviews "imdb_master.csv" (Left Model.vocabLen)
               encodeReview <- reviewEncoder dict
               model <- decodeFile "model"
               forever $ getLine >>= encodeReview >>= print . test model . U.toList

train :: (String, String) -> [String] -> IO ()
train (host, port) seeds =
        do modelExists <- doesFileExist "model"
           modelVar <- if modelExists
                       then decodeFile "model" >>= newMVar
                       else newEmptyMVar
           validVar <- newEmptyMVar
           reviewExists <- doesFileExist "reviews"
           (trainSet, validSet) <- if reviewExists
                                  then decodeFile "reviews"
                                  else fmap fst initializeReviews
           trainList <- shuffledSet trainSet
           testValidSet <- fmap (V.take 5000 . head) $ shuffledSet validSet
           let batches = map (\set -> [V.slice i 20 set | i <- [0, 20 .. V.length set - 19]])
                             trainList
               testTrainSet = V.take 5000 $ head trainList
           if null host
           then do gradVar <- newEmptyMVar
                   _ <- forkIO $ computationThread Nothing gradVar modelVar validVar 0 batches
                   let go = 
                        do maybeGrad <- liftIO $ tryTakeMVar gradVar
                           case maybeGrad of
                                Just grad -> liftIO $ modifyMVar_ modelVar (return . Model.update grad)
                                Nothing -> return ()
                           go
                   _ <- forkIO go
                   return ()
           else do P2P.bootstrapNonBlocking host
                                            port
                                            (\_ -> (host, port))
                                            initRemoteTable
                                            (map P2P.makeNodeId seeds)
                                            (mainProcess modelVar validVar batches)
                   return ()
           logHandle <- openFile "log" AppendMode
           hSetBuffering logHandle LineBuffering
           let go bestScore = do model <- takeMVar validVar
                                 let trainScore = 100 * (1 - validate testTrainSet (V.length testTrainSet) model)
                                     validScore = 100 * (1 - validate testValidSet (V.length testValidSet) model)
                                 hPutStrLn logHandle $ show trainScore ++ " " ++ show validScore
                                 putStrLn $ show trainScore ++ "% " ++ show validScore ++ "%"
                                 encodeFile "model" model
                                 if validScore >= bestScore
                                 then do copyFile "model" ("model" ++ show validScore)
                                         go validScore
                                 else go bestScore
           go 0

joinNetwork :: Process (Maybe [ProcessId])
joinNetwork =
        do masterProcesses <- getCapable "master"

           liftIO $ putStrLn "Connessione..."
           case masterProcesses of
                (master : []) -> do liftIO $ putStrLn "Trovato un master"
                                    joinRequest master
                (master : ms) -> do liftIO $ putStrLn "Trovati più master"
                                    liftIO $ threadDelay (5000000 + 1000000 * length ms)
                                    joinNetwork
                [] -> do rid <- liftIO randomIO :: Process Int
                         liftIO $ putStrLn "Modalità master"
                         getSelfPid >>= register "master"
                         liftIO $ threadDelay 3000000
                         getCapable "master" >>= mapM_ (flip send rid)

                         let ridLoop = do mrid' <- expectTimeout 1000000
                                          case mrid' of
                                               Nothing -> return True
                                               Just rid' | rid' > rid -> return False
                                               _ -> do liftIO $ putStrLn "In attesa degli altri master..."
                                                       ridLoop
                         winner <- ridLoop
                         if winner
                         then do liftIO $ putStrLn "Modalità master confermata"
                                 liftIO $ threadDelay 5000000
                                 let getPorts =
                                        do mport <- expectTimeout 0
                                           case mport of
                                                Just port -> fmap (port :) getPorts
                                                Nothing -> return []
                                 ports <- getPorts

                                        
                                 oldActive <- fmap length $ getCapable "active"
                                 oldPassive <- fmap length $ getCapable "passive"
                                 let newPassive = (oldActive - oldPassive + length ports + 1) `quot` 2
                                     sendList = map sendChan ports
                                     (sendPassive, sendActive) =
                                             splitAt newPassive
                                                     (registerActive : sendList)
                                 mapM_ ($ False) sendPassive
                                 mapM_ ($ True) sendActive
                                 unregister "master"
                                 if newPassive == 0
                                 then fmap Just $ getCapable "passive"
                                 else return Nothing
                         else do liftIO $ putStrLn "Modalità master annullata"
                                 unregister "master"
                                 joinNetwork
        where joinRequest master = do (sendPort, rcvPort) <- newChan
                                      send master sendPort
                                      mActive <- receiveChanTimeout 12000000 rcvPort
                                      case mActive of
                                           Just active -> do registerActive active
                                                             if active
                                                             then fmap Just $ getCapable "passive"
                                                             else return Nothing
                                           Nothing -> joinNetwork

              registerActive True = do liftIO $ putStrLn "Modalità attiva"
                                       getSelfPid >>= register "active"
              registerActive False = do liftIO $ putStrLn "Modalità passiva"
                                        getSelfPid >>= register "passive"

leaveNetwork :: Bool -> Process ()
leaveNetwork True = unregister "active"
leaveNetwork False = unregister "passive"

mainProcess :: MVar Model -> MVar Model -> [[V.Vector Review]] -> Process ()
mainProcess modelVar validVar reviews =
        do gradVar <- liftIO newEmptyMVar
           _ <- liftIO . forkIO $ computationThread Nothing gradVar modelVar validVar 0 reviews

           let go peers = 
                do maybeGrad <- liftIO $ tryTakeMVar gradVar
                   case maybeGrad of
                        Just grad -> liftIO $ modifyMVar_ modelVar (return . Model.update grad)
                        Nothing -> return ()
                   maybeModel <- liftIO $ tryReadMVar modelVar
                   maybePeerModel <- exchange peers maybeModel
                   case (maybeModel, maybePeerModel) of
                        (Just model, Just peerModel) ->
                                do _ <- liftIO $ swapMVar modelVar $ Model.average model peerModel
                                   liftIO $ putStrLn "Modello aggiornato"
                                   n <- liftIO $ randomRIO (0, 4) :: Process Int
                                   if n == 0 && isJust peers
                                   then getCapable "passive" >>= go . Just
                                   else go peers
                                   go peers
                        (Nothing, Just peerModel) -> do liftIO $ putMVar modelVar peerModel
                                                        liftIO $ putStrLn "Modello ricevuto"
                                                        go peers
                        (_, Nothing) -> do leaveNetwork $ isJust peers
                                           liftIO $ putStrLn "Riconnessione..."
                                           peers' <- joinNetwork
                                           liftIO $ randomRIO (500000, 2000000) >>= threadDelay
                                           go peers'
           joinNetwork >>= go

exchange :: Maybe [ProcessId] -> Maybe Model -> Process (Maybe Model)
exchange Nothing model = do rcvd <- expectTimeout 60000000
                            case rcvd of
                                 Just (peerModel, sendPort) ->
                                         do sendChan sendPort model
                                            return peerModel
                                 Nothing -> do liftIO $ putStrLn "Timeout"
                                               return Nothing
exchange (Just []) _ = liftIO (putStrLn "Nessun peer a cui inviare") >> return Nothing
exchange (Just peers) model = do n <- liftIO $ randomRIO (0, length peers - 1)
                                 (sendPort, rcvPort) <- newChan
                                 send (peers !! n) (model, sendPort)
                                 fmap join $ receiveChanTimeout 20000000 rcvPort

computationThread :: Maybe Model
                  -> MVar Model
                  -> MVar Model
                  -> MVar Model
                  -> Int
                  -> [[V.Vector Review]]
                  -> IO ()
computationThread velocity gradVar modelVar validVar _ ([] : sets) =
        do readMVar modelVar >>= putMVar validVar
           computationThread velocity gradVar modelVar validVar 0 sets
computationThread velocity gradVar modelVar validVar cur ((batch : reviews) : sets) =
        do model <- readMVar modelVar
           maybeGrad <- tryReadMVar gradVar
           let model' | Just grad <- maybeGrad = Model.update grad model
                      | otherwise = model
               grad = Model.gradient batch (V.length batch) velocity model'
           evaluate grad
           putMVar gradVar grad
           liftIO . putStrLn $ "Batch " ++ show cur ++ " completato"
           computationThread (Just grad) gradVar modelVar validVar (cur + 1) (reviews : sets)

shuffledSet :: V.Vector a -> IO [V.Vector a]
shuffledSet v = do stdGen <- newStdGen
                   return . map fst . tail $ iterate (uncurry shuffle) (v, stdGen)

getCapable :: String -> Process [ProcessId]
getCapable service = do (sp, rp) <- newChan
                        P2P.nsendPeers "P2P:Controller" (service, sp)
                        go rp []

    where go rp acc = do res <- receiveChanTimeout 1000000 rp
                         case res of
                              Just pid -> go rp (pid:acc)
                              Nothing -> return acc
