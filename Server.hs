{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Server where
import Message
import Data.Aeson
import Data.Time
import System.Random
import Data.HashMap.Lazy

-- Command (<Key> <Value>) (to put)

data ServerState = Follower | Candidate | Leader deriving (Show)


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
  log :: [Command],
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
receiveMessage s time (Just m@Message{..}) = s
    -- where s' = s { lastReceived = time }
    --       responded = respondToMessage s
    --       response = (constructMessage s) <$> (sendMe s')


-- respondToMessage :: Server -> RMessage -> Server
-- respondToMessage s@Server{..} (Just rm)
  -- | sState == Candidate = reactCandidate s rm
  -- | sState == Follower = reactFollower s rm
  -- | sState == Leader = reactLeader s rm

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







