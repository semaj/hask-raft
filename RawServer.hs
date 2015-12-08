
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
import qualified Data.HashMap.Lazy as HM
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
    msg <- recv s 4096
    -- putStrLn "MESSAGE!"
    -- putStrLn msg
    let splitR = splitOn "\n" msg
    -- putStrLn $ "split: " ++ (show splitR)
    let fsMessages = map fromString splitR
    -- putStrLn $ "fsm: " ++ (show fsMessages)
    let mMessages = map decode fsMessages :: [Maybe Message]
    -- putStrLn $ "mmess: " ++ (show mMessages)
    writeList2Chan messages $ catMaybes mMessages

getSocket :: String -> IO Socket
getSocket id = do
  soc <- socket AF_UNIX Stream 0
  connect soc $ SockAddrUnix id
  return soc

sender :: Socket -> Chan Message -> IO ()
sender s messages = do
  forever $ do
    threadDelay 1000
    msg <- getChanContents messages
    let fixed = map (\x -> ((toString . encode) x) ++ "\n") msg
    mapM (send s) fixed

serverLoop :: Server -> Chan Message -> Chan Message -> Socket -> IO ()
serverLoop server receiving sending socket = do
  message <- tryGet receiving
  time <- getCurrentTime
  possibleTimeout <- getStdRandom $ randomR timeoutRange
  newMid <- getStdRandom $ randomR (100000, 999999)
  -- unless (isNothing message) $ do putStrLn $ show $ fromJust message
  let server' = step (show (newMid :: Int)) $ receiveMessage server time possibleTimeout message
  putStrLn $ show $ (show $ sState server') ++ " : " ++ (sid server') ++ " : " ++ (show $ currentTerm server') ++ " | " ++ (show $ votedFor server')
  when ((length $ sendMe server') > 0) $ do
    --let mapped = map (((flip (++)) "\n") . toString . encode) $ sendMe server'
    putStrLn $ "to : " ++ (show $ map dst $ sendMe server')
    -- putStrLn $ show mapped
    writeList2Chan sending $ sendMe server'
    --void $ mapM (send socket) mapped
  --threadDelay 100
  serverLoop (server' { sendMe = [] } ) receiving sending socket

start :: Server -> Socket -> IO ()
start server socket = do
  receiving <- newChan
  sending <- newChan
  tid <- forkIO $ receiver socket receiving
  stid <- forkIO $ sender socket sending
  serverLoop server receiving sending socket
  killThread tid
  killThread stid

initialServer :: String -> [String] -> IO Server
initialServer myID otherIDs = do
  timeout <- getStdRandom $ randomR timeoutRange
  time <- getCurrentTime
  return $ initServer myID otherIDs time timeout

main :: IO ()
main = do
    args <- getArgs
    let myID = head args
        otherIDs = tail args
    server <- initialServer myID otherIDs
    withSocketsDo $ bracket (getSocket myID) sClose (start server)
