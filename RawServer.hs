
module Main where
import Message
import Server

import Network.Socket
import System.Environment
import Control.Exception
import Data.Aeson
import Data.ByteString.Lazy.UTF8 (fromString, toString)
import Data.List.Split
import Control.Concurrent
import Control.Monad
import Data.Maybe
import System.Random
import Data.Time
import Data.List
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

tryGet :: Chan a -> IO (Maybe a)
tryGet chan = do
  empty <- isEmptyChan chan
  if empty then
    return Nothing
  else do
    response <- readChan chan
    return $ Just response

receiver :: Socket -> Chan Message -> IO ()
receiver s messages = do
  forever $ do
    msg <- recv s 8192
    --putStrLn "MESSAGE!"
    --putStrLn msg
    let splitR = splitOn "\n" msg
    -- putStrLn $ "split: " ++ (show splitR)
    let fsMessages = map fromString splitR
    -- putStrLn $ "fsm: " ++ (show fsMessages)
    let mMessages = map decode fsMessages :: [Maybe Message]
    --putStrLn $ "mmess: " ++ (show mMessages)
    writeList2Chan messages $ catMaybes mMessages

getSocket :: String -> IO Socket
getSocket id = do
  soc <- socket AF_UNIX Stream defaultProtocol
  connect soc $ SockAddrUnix id
  return soc

serverLoop :: Server -> Chan Message -> Socket -> IO ()
serverLoop server chan socket = do
  message <- tryGet chan
  time <- getCurrentTime
  possibleTimeout <- getStdRandom $ randomR timeoutRange
  newMid <- getStdRandom $ randomR (100000, 999999)
  -- when (sState server == Leader) $ do putStrLn $ sid server
  let server' = step (show (newMid :: Int)) time $ receiveMessage server time possibleTimeout message
      mapped = map (((flip (++)) "\n") . toString . encode) $ nub $ sendMe server'
      fails = filter ((== FAIL) . messType) $ sendMe server'
  when (length fails > 0) $ do putStrLn $ show fails
  -- when (sState server' /= Leader && (length $ sendMe server') > 0) $ do putStrLn $ show $ nub $ map messType $ sendMe server'
  mapM (send socket) mapped
  serverLoop (server' { sendMe = [] } ) chan socket

start :: Server -> Chan Message -> Socket -> IO ()
start server chan socket = do
  tid <- forkIO $ receiver socket chan
  serverLoop server chan socket
  killThread tid

initialServer :: String -> [String] -> IO Server
initialServer myID otherIDs = do
  timeout <- getStdRandom $ randomR timeoutRange
  time <- getCurrentTime
  return $ initServer myID otherIDs time timeout

main :: IO ()
main = do
    args <- getArgs
    messageChan <- newChan
    let myID = head args
        otherIDs = tail args
    server <- initialServer myID otherIDs
    withSocketsDo $ bracket (getSocket myID) sClose (start server messageChan)
