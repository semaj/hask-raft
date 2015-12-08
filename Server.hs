{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Server where
import Message
import Data.Aeson
import Data.Time
import System.Random
import Debug.Trace
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Maybe

data ServerState = Follower | Candidate | Leader deriving (Show, Eq)

-- These (!)s just force strict data types.
-- Nothing to worry about. >:)
data Server = Server {
  sState :: !ServerState,
  sid :: !String,
  others :: [String],
  store :: HM.HashMap String String,
  sendMe :: [Message],
  -- Persistent state
  currentTerm :: Int,
  votedFor :: !String,
  slog :: [Command],
  -- Volatile state
  commitIndex :: Int,
  lastApplied :: Int,
  -- Only on leaders
  nextIndices :: HM.HashMap String Int,
  matchIndices :: HM.HashMap String Int,
  -- Only on candidates
  votes :: HS.HashSet String,
  --
  timeout :: Int, -- ms
  lastReceived :: UTCTime
} deriving (Show)

initServer :: String -> [String] -> UTCTime -> Int -> Server
initServer myID otherIDs time timeout = Server { sid = myID,
                                                 others = otherIDs,
                                                 sState = Follower,
                                                 store = HM.empty,
                                                 sendMe = [],
                                                 currentTerm = 0,
                                                 votedFor = "",
                                                 slog = [],
                                                 commitIndex = 0,
                                                 lastApplied = 0,
                                                 nextIndices = HM.fromList $ map (\x -> (x, 0)) otherIDs,
                                                 matchIndices = HM.fromList $ map (\x -> (x, 0)) otherIDs,
                                                 votes = HS.empty,
                                                 timeout = timeout,
                                                 lastReceived = time }


majority :: Int
majority = 2 -- because 1 is always implied (1 + 2)

append :: a -> [a] -> [a]
append a as = as ++ [a]

step :: String -> Server -> Server
step newMid s@Server{..}
  | sState == Follower = stepFollower newMid s
  | sState == Candidate = stepCandidate newMid s
  | sState == Leader = stepLeader newMid s

stepFollower :: String -> Server -> Server
stepFollower newMid s@Server{..} = s

stepCandidate :: String -> Server -> Server
stepCandidate newMid s@Server{..}
  | HS.size votes >= majority = s { sState = Leader,
                                   votedFor = sid,
                                   nextIndices = HM.map (const $ length slog) nextIndices,
                                   matchIndices = HM.map (const 0) matchIndices,
                                   votes = HS.empty }
  | otherwise = s { sendMe = append rv sendMe } -- could avoid sending to those already voted for us
    where lastLogIndex = if length slog == 0 then 0 else length slog - 1
          lastLogTerm = if length slog == 0 then 0 else cterm $ last slog
          rv = Message sid "FFFF" "FFFF" RAFT newMid Nothing Nothing $ Just $ RV currentTerm sid lastLogIndex lastLogTerm

stepLeader :: String -> Server -> Server
stepLeader newMid s@Server{..} = s

isExpired :: UTCTime -> UTCTime -> Int -> Bool
isExpired lastReceived now timeout = diff > timeout'
  where timeout' = 0.001 * realToFrac timeout
        diff = abs $ diffUTCTime now lastReceived

receiveMessage :: Server -> UTCTime -> Int -> Maybe Message -> Server
receiveMessage s@Server{..} time newTimeout Nothing
  | isExpired lastReceived time timeout = s { sState = Candidate,
                                              timeout = newTimeout,
                                              lastReceived = time,
                                              votes = HS.empty,
                                              currentTerm = currentTerm + 1 }
  | otherwise = s
receiveMessage _ _ _ m | trace (show m) False = undefined
receiveMessage s time _ (Just m@Message{..})
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
    where mRvr isSuccess = Message sid src (if isSuccess then src else "FFFF") RAFT mid Nothing Nothing (Just $ RVR (if isSuccess then term else currentTerm) isSuccess)
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

