{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Server where
import Message
import Data.Aeson
import Data.Time
import System.Random
import Data.HashMap.Lazy as HM
import Data.HashSet as HS
import Data.Maybe

-- Command (<Key> <Value>) (to put)

data ServerState = Follower | Candidate | Leader deriving (Show, Eq)

-- These (!)s just force strict data types.
-- Nothing to worry about. >:)
data Server = Server {
  sState :: !ServerState,
  sid :: !String,
  others :: [String],
  store :: HashMap String String,
  sendMe :: [Message],
  -- Persistent state
  currentTerm :: Int,
  votedFor :: !String,
  slog :: [Command],
  -- Volatile state
  commitIndex :: Int,
  lastApplied :: Int,
  -- Only on leaders
  nextIndices :: [Int],
  matchIndices :: [Int],
  -- Only on candidates
  votes :: HashSet String,
  --
  timeout :: Int, -- ms
  lastReceived :: UTCTime
}

append :: a -> [a] -> [a]
append a as = as ++ [a]

step :: Server -> Server
step = id

isExpired :: UTCTime -> UTCTime -> Int -> Bool
isExpired lastReceived now timeout = diff > timeout'
  where timeout' = 0.001 * realToFrac timeout
        diff = abs $ diffUTCTime now lastReceived

maybeTimeout :: Server -> UTCTime -> IO Server
maybeTimeout s@Server{..} time
  | isExpired lastReceived time timeout = resetTimeout s
  | otherwise = return s

receiveMessage :: Server -> UTCTime -> Maybe Message -> Server
receiveMessage s _ Nothing = s
receiveMessage s time (Just m@Message{..})
  | messType ==  GET = respondGet s' m
  | messType == PUT = respondPut s' m
  | messType == RAFT = respondRaft s' m
  | otherwise = s
      where s' = s { lastReceived = time }
    --       responded = respondToMessage s
    --       response = (constructMessage s) <$> (sendMe s')

respondGet :: Server -> Message -> Server
respondGet s@Server{..} m@Message{..}
  | sState == Leader = s { slog = append command slog }
  | otherwise = s { sendMe = append redirect sendMe }
    where command = Command CGET currentTerm src (fromJust key) ""
          redirect = Message sid src votedFor REDIRECT mid Nothing Nothing Nothing

respondPut :: Server -> Message -> Server
respondPut s@Server{..} m@Message{..}
  | sState == Leader = s { slog = append command slog }
  | otherwise = s { sendMe = append redirect sendMe }
    where command = Command CPUT currentTerm src (fromJust key) (fromJust value)
          redirect = Message sid src votedFor REDIRECT mid Nothing Nothing Nothing

respondRaft :: Server -> Message -> Server
respondRaft s@Server{..} m@Message{..}
  | sState == Follower = respondFollower s m $ fromJust rmess
  | sState == Candidate = respondCandidate s m $ fromJust rmess
  | otherwise = respondLeader s m $ fromJust rmess

upToDate :: [Command] -> Int -> Int -> Bool
upToDate [] _ _ = True
upToDate base lastLogTerm lastLogIndex = baseTI <= (lastLogTerm, lastLogIndex)
    where baseTI = (cterm $ last base, length base)

respondFollower :: Server -> Message -> RMessage -> Server
respondFollower s@Server{..} m@Message{..} r@RV{..}
  | term <= currentTerm = s { sendMe = append (mRvr False) sendMe }
  | upToDate slog lastLogTerm lastLogIndex = s { sendMe = append (mRvr True) sendMe,
                                                 votedFor = candidateId,
                                                 currentTerm = term }
  | otherwise = s { sendMe = append (mRvr False) sendMe }
    where mRvr isSuccess = Message sid src votedFor RAFT mid Nothing Nothing (Just $ RVR currentTerm isSuccess)
respondFollower s _ _ = s

-- respondFollower s@Server{..} m@Message{..} r@AE{..}
--   | term < currentTerm = fail
--   | (length slog - 1) < prevLogIndex = fail
--   | cterm (slog!!prevLogIndex) != prevLogTerm = fail { slog = take toTake slog }
--   -- this is not quite right.
--   | otherwise = succeed { commitIndex = newCommit, slog = slog ++ entries }
--       where aer isSuccess = AER currentTerm isSuccess
--             mAer isSuccess = Message sid src votedFor RAFT mid Nothing Nothing (Just $ aer isSuccess)
--             succeed = s { sendMe = append (mAer True) sendMe }
--             fail = s { sendMe = append (mAer False) sendMe }
--             toTake = (length slog) - (prevLogIndex - 1)
--             newCommit = if leaderCommit > commitIndex then min (length entries - 1) leaderCommit else commitIndex

respondLeader :: Server -> Message -> RMessage -> Server
respondLeader s@Server{..} m@Message{..} _ = s

respondCandidate :: Server -> Message -> RMessage -> Server
respondCandidate s@Server{..} m@Message{..} r@RVR{..}
  | voteGranted == True = s { votes = HS.insert src votes }
  | otherwise = s
respondCandidate s@Server{..} m@Message{..} r@AE{..}
  | term >= currentTerm = s { sState = Follower,
                             currentTerm = term,
                             votedFor = src }
  | otherwise = s
respondCandidate s _ _ = s

resetTimeout :: Server -> IO Server
resetTimeout server = do
  newTimeout <- getStdRandom $ randomR timeoutRange
  newStarted <- getCurrentTime
  return server { timeout = newTimeout, lastReceived = newStarted }

updateTimeout :: Server -> IO Server
updateTimeout server = do
  newStarted <- getCurrentTime
  return server { lastReceived = newStarted }







