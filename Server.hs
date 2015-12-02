{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Server where
import Data.Aeson
import Data.Time
import System.Random
import Data.HashMap.Lazy

type Command = (String, String)
-- Command (<Key> <Value>) (to put)

data ServerState = Follower | Candidate | Leader deriving (Show)

timeoutRange :: (Int, Int)
timeoutRange = (150, 300) -- ms

-- These (!)s just force strict data types.
-- Nothing to worry about. >:)
data Server = Server {
  sState :: !ServerState,
  id :: !String,
  others :: [String],
  store :: HashMap String String,
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
  -- Only on candidates
  votes :: [Int]
  --
  timeout :: Int, -- seconds
  started :: UTCTime
}

respondToMessage :: Server -> UTCTime -> Maybe Message -> Server
respondToMessage s _ Nothing = s
respondToMessage s@Server{..} time (Just m)
  | sState == Candidate = reactCandidate s' m
  | sState == Follower = reactFollower s' m
  | sState == Leader = reactLeader s' m
    where s' = s { started = time }

reactCandidate :: Server -> Message -> Server
reactCandidate s@Server{..} m@RVR{..}
  | voteGranted == True = s { votes = source:votes }
  | otherwise = s
reactCandidate s@Server{..} m@AEM{..}
  | term >= currentTerm = toFollower s source
  | otherwise = s
reactCandidate s _ = s

toFollower :: Server -> String -> Server
toFollower s ss = s
-- possible optimization here: if receive a RVM, check the term. if it's higher
-- or the same (and there are more votes, add this field) then they are the leader

reactFollower :: Server -> Message -> Server
reactFollower s@Server{..} m@Message{..} = undefined

reactLeader :: Server -> Message -> Server
reactLeader s@Server{..} m@Message{..} = undefined

resetTimeout :: Server -> IO Server
resetTimeout server = do
  newTimeout <- getStdRandom $ randomR timeoutRange
  newStarted <- getCurrentTime
  return server { timeout = newTimeout, started = newStarted }

updateTimeout :: Server -> IO Server
updateTimeout server = do
  newStarted <- getCurrentTime
  return server { started = newStarted }







