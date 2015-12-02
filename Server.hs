{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Server where
import GHC.Generics
import Data.Aeson
import Data.Time
import System.Random

data Command = Command String String deriving (Generic, Show)
-- Command <Key> <Value> (to put)

instance ToJSON Command
instance FromJSON Command

data ServerState = Follower | Candidate | Leader deriving (Show)

timeoutRange :: (Int, Int)
timeoutRange = (150, 300) -- ms

-- These (!)s just force strict data types.
-- Nothing to worry about.
data Server = Server {
  sState :: !ServerState,
  id :: !String,
  others :: [String],
  -- Persistent state
  currentTerm :: Int,
  votedFor :: !String,
  log :: [Command],
  -- Volatile state
  commitIndex :: Int,
  lastApplied :: Int,
  -- Only on leaders
  nextIndices :: [Int],
  matchIndices :: [Int],

  timeout :: Int, -- seconds
  started :: !UTCTime
}

resetTimeout :: Server -> IO Server
resetTimeout server = do
  newTimeout <- getStdRandom $ randomR timeoutRange
  newStarted <- getCurrentTime
  return server { timeout = newTimeout, started = newStarted }

updateTimeout :: Server -> IO Server
updateTimeout server = do
  newStarted <- getCurrentTime
  return server { started = newStarted }







