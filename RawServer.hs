{-# LANGUAGE OverloadedStrings #-}

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
import Data.HashMap.Lazy as HM hiding (map)
import Data.HashSet as HS hiding (map)

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
    let splitR = splitOn "\n" msg
        mMessages = map (decode . fromString) splitR :: [Maybe Message]
    writeList2Chan messages $ catMaybes mMessages

getSocket :: String -> IO Socket
getSocket id = do
  soc <- socket AF_UNIX Stream 0
  connect soc $ SockAddrUnix id
  return soc

serverLoop :: Server -> Chan Message -> Socket -> IO ()
serverLoop server chan socket = do
  message <- tryGet chan
  time <- getCurrentTime
  unless (isNothing message) $ do putStrLn $ show $ fromJust message
  let server' = step $ receiveMessage server time message
  mapM (send socket . toString . encode) $ sendMe server'
  server'' <- maybeTimeout server' time
  serverLoop server' chan socket

start :: Server -> Chan Message -> Socket -> IO ()
start server chan socket = do
  tid <- forkIO $ receiver socket chan
  serverLoop server chan socket
  killThread tid

initialServer :: String -> [String] -> IO Server
initialServer myID otherIDs = do
  timeout <- getStdRandom $ randomR timeoutRange
  lastReceived <- getCurrentTime
  return $ Server Candidate myID otherIDs HM.empty [] 0 "" [] 0 0 [] [] HS.empty timeout lastReceived

main :: IO ()
main = do
    args <- getArgs
    messageChan <- newChan
    let myID = head args
        otherIDs = tail args
    server <- initialServer myID otherIDs
    withSocketsDo $ bracket (getSocket myID) sClose (start server messageChan)
