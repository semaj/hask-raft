module Main where
import Message
import Utils
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

-- This function runs in its own thread, receiving messages and adding them to a "channel"
receiver :: Socket -> Chan Message -> IO ()
receiver s messages = do
  forever $ do
    msg <- recv s 8192
    let splitR = splitOn "\n" msg
    let fsMessages = map fromString splitR
    let mMessages = map decode fsMessages :: [Maybe Message]
    writeList2Chan messages $ catMaybes mMessages

getSocket :: String -> IO Socket
getSocket id = do
  soc <- socket AF_UNIX Stream defaultProtocol
  connect soc $ SockAddrUnix id
  return soc

-- This is the main server loop. It attempts to read a message from the
-- channel, then steps it
serverLoop :: Server -> Chan Message -> Socket -> IO ()
serverLoop server chan socket = do
  message <- tryGet chan
  time <- getCurrentTime
  possibleTimeout <- getStdRandom $ randomR timeoutRange
  newMid <- getStdRandom $ randomR (100000, 999999)
  -- This is where the server receives the message and then responds appropriately
  let server' = step (show (newMid :: Int)) time $ receiveMessage server time possibleTimeout message
      mapped = map (((flip (++)) "\n") . toString . encode) $ sendMe server'
  mapM (send socket) mapped
  serverLoop (server' { sendMe = [] } ) chan socket -- recursive

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
