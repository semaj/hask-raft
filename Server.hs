{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Server where
import Message
import Data.Aeson
import Data.Time
import System.Random
import Data.HashMap.Lazy
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
  votes :: [Int],
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
  | otherwise = s -- this shouldn't happen
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
  | sState == Follower = respondFollower m rmess
  | sState == Candidate = respondCandidate m rmess
  | otherwise = respondCandidate m rmess

respondFollower :: Server -> Messsage -> RMessage -> Server
respondFollower s@Server{..} m@Message{..} r@AE{..}
  | term < currentTerm = fail
  | (length slog - 1) < prevLogIndex = fail
  | cterm (slog!!prevLogIndex) != prevLogTerm = fail { slog = take toTake slog }
  -- this is not quite right.
  | otherwise = succeed { commitIndex = newCommit, slog = slog ++ entries }
      where aer isSuccess = AER currentTerm isSuccess
            mAer isSuccess = Message sid src votedFor RAFT mid Nothing Nothing (Just $ aer isSuccess)
            succeed = s { sendMe = append (mAer True) sendMe }
            fail = s { sendMe = append (mAer False) sendMe }
            toTake = (length slog) - (prevLogIndex - 1)
            newCommit = if leaderCommit > commitIndex then min (length entries - 1) leaderCommit else commitIndex


-- reactCandidate :: Server -> RMessage -> Server
-- reactCandidate s@Server{..} m@RVR{..}
--   | voteGranted == True = s { votes = source:votes }
--   | otherwise = s
-- reactCandidate s@Server{..} m@AEM{..}
--   | term >= currentTerm = toFollower s source
--   | otherwise = s
-- reactCandidate s _ = s
-- -- possible optimization here: if receive a RVM, check the term. if it's higher
-- -- or the same (and there are more votes, add this field) then they are the leader

-- toFollower :: Server -> String -> Server
-- toFollower s ss = s

-- reactFollower :: Server -> RMessage -> Server
-- reactFollower s@Server{..} m@AEM{..} = s -- unimplemented
-- reactFollower s@Server{..} m@RVM{..}
--   |

-- reactLeader :: Server -> RMessage -> Server
-- reactLeader s@Server{..} m@RMessage{..} = undefined

resetTimeout :: Server -> IO Server
resetTimeout server = do
  newTimeout <- getStdRandom $ randomR timeoutRange
  newStarted <- getCurrentTime
  return server { timeout = newTimeout, lastReceived = newStarted }

updateTimeout :: Server -> IO Server
updateTimeout server = do
  newStarted <- getCurrentTime
  return server { lastReceived = newStarted }







