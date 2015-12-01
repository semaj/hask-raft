{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Server where
import GHC.Generics
import Data.Aeson

data Command = Command String String deriving (Generic, Show)
-- Command <Key> <Value> (to put)

instance ToJSON Command
instance FromJSON Command

data ServerState = Follower | Candidate | Leader deriving (Show)

-- These (!)s just force strict data types.
-- Nothing to worry about.
data Server = Server {
  sState :: ServerState,
  -- Persistent state
  currentTerm :: Int,
  votedFor :: !String,
  log :: [Command],
  -- Volatile state
  commitIndex :: Int,
  lastApplied :: Int,
  -- Only on leaders
  nextIndices :: [Int],
  matchIndices :: [Int]
}



