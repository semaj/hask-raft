{-# LANGUAGE OverloadedStrings #-}

module Main where
import Message
import Server

import Network.Socket
import System.Environment
import Control.Exception
import Data.Aeson
import Data.ByteString.Lazy.UTF8 (fromString)

getSocket :: String -> IO Socket
getSocket id = do
  soc <- socket AF_UNIX Stream 0
  connect soc $ SockAddrUnix id
  return soc

start :: Socket -> IO ()
start s = do
  send s "hello world!"
  r <- recv s 1024
  let mMessage = decode (fromString r) :: Maybe Message
  putStrLn $ show mMessage
  start s


main :: IO ()
main = do
    args <- getArgs
    let myID = head args
        otherIDS = tail args
    withSocketsDo $ bracket (getSocket myID) sClose start
