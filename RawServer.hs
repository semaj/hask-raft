{-# LANGUAGE OverloadedStrings #-}

module Main where
import Message
import Server

import Network.Socket
import System.Environment
import Control.Exception
import Data.Aeson
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.List.Split

receiver :: Socket -> Chan Message -> IO ()
receiver s messages = do
  forever $ do
    msg <- recv s 32768
    let splitR = splitOn "\n" messages
        mMessages = map (decode . fromString) splitR :: [Maybe Message]
    writeList2Chan messages $ catMaybes mMessages

getSocket :: String -> IO Socket
getSocket id = do
  soc <- socket AF_UNIX Stream 0
  connect soc $ SockAddrUnix id
  return soc

start :: Server -> Socket -> IO ()
start server socket = do
  send socket "hello world!"
  r <- recv socket 2048
  let splitR = splitOn "\n" r
      mMessages = map (decode . fromString) splitR :: [Maybe Message]
  mapM (putStrLn . show) mMessages
  -- map processMessage mMessages
  start server socket


main :: IO ()
main = do
    args <- getArgs
    let myID = head args
        otherIDs = tail args
    server <- resetTimeout $ Server Candidate 0 myID otherIDs "" [] 0 0 [] [] 0
    withSocketsDo $ bracket (getSocket myID) sClose (start server)
