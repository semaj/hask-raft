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
import Data.List

data ServerState = Follower | Candidate | Leader deriving (Show, Eq)

-- These (!)s just force strict data types.
-- Nothing to worry about. >:)
data Server = Server {
  sState :: !ServerState,
  sid :: !String,
  others :: [String],
  store :: HM.HashMap String String,
  sendMe :: [Message],
  lastMess :: HM.HashMap String SentMessage,
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
  lastReceived :: UTCTime,
  lastSent :: UTCTime
} deriving (Show)

initServer :: String -> [String] -> UTCTime -> Int -> Server
initServer myID otherIDs time timeout = Server { sid = myID,
                                                 others = otherIDs,
                                                 sState = Follower,
                                                 store = HM.empty,
                                                 lastMess = HM.empty,
                                                 sendMe = [],
                                                 currentTerm = 0,
                                                 votedFor = "FFFF",
                                                 slog = [],
                                                 commitIndex = -1,
                                                 lastApplied = -1,
                                                 nextIndices = HM.fromList $ map (\x -> (x, 0)) otherIDs,
                                                 matchIndices = HM.fromList $ map (\x -> (x, 0)) otherIDs,
                                                 votes = HS.empty,
                                                 timeout = timeout,
                                                 lastReceived = time,
                                                 lastSent = time }

majority :: Int
majority = 2 -- because 1 is always implied (1 + 2)

push :: a -> [a] -> [a]
push a as = as ++ [a]

step :: String -> UTCTime -> Server -> Server
step newMid now s@Server{..}
  | sState == Follower = stepFollower newMid s
  | sState == Candidate = stepCandidate newMid now s
  | sState == Leader = stepLeader newMid now s

stepFollower :: String -> Server -> Server
stepFollower newMid s@Server{..} = s

resendOutdated :: UTCTime -> HM.HashMap String SentMessage -> (HM.HashMap String SentMessage, [Message])
resendOutdated now hm = (update, getExpired)
    where expired x = 0.001 < (abs $ diffUTCTime now (sent x))
          getExpired = map message $ HM.elems $ HM.filter (\y -> expired y) hm
          update = HM.map (\z -> if expired z then z { sent = now } else z) hm

sendNew :: UTCTime
        -> Message -- Base message
        -> HM.HashMap String SentMessage
        -> [String]-- destinations
        -> (HM.HashMap String SentMessage, [Message])
sendNew now Message{..} hm dests = (newMap, HM.elems destToMess)
    where newDests = filter (\x -> not $ HM.member x hm) dests
          destToMess = HM.fromList $ map (\x -> (x, Message src x leader messType (mid ++ x) Nothing Nothing rmess)) newDests
          newMap = foldl (\a b -> HM.insert b (SentMessage ((HM.!) destToMess b) now) a) hm newDests

stepCandidate :: String -> UTCTime -> Server -> Server
stepCandidate newMid now s@Server{..}
  | HS.size votes >= majority = s { sState = Leader,
                                   votedFor = sid,
                                   lastMess = HM.empty,
                                   nextIndices = HM.map (const $ length slog) nextIndices,
                                   matchIndices = HM.map (const 0) matchIndices,
                                   votes = HS.empty }
  | otherwise = s { sendMe = sendMe ++ resend ++ alsoSend, lastMess = newMap }
    where lastLogIndex = if length slog == 0 then 0 else length slog - 1
          lastLogTerm = if length slog == 0 then 0 else cterm $ last slog
          rv = Just $ RV currentTerm sid lastLogIndex lastLogTerm
          base = Message sid "FFFF" "FFFF" RAFT newMid Nothing Nothing rv
          stillNeed = HS.toList $ HS.difference (HS.fromList others) votes
          (resentMap, resend) = resendOutdated now lastMess
          (newMap, alsoSend) = sendNew now base resentMap stillNeed

-- Execute commands (that we can), while queueing up responses, and send AEs
stepLeader :: String -> UTCTime -> Server -> Server
stepLeader newMid now s@Server{..} = leaderSendAEs newMid now $ leaderExecute s

-- Get the AEs needed to send for the next round
leaderSendAEs :: String -> UTCTime -> Server -> Server
leaderSendAEs newMid now s@Server{..}
  | 0.001 > (abs $ diffUTCTime lastSent now) = s
  | otherwise = s { sendMe = sendMe ++ toFollowers, lastSent = now }
  where toFollowers = map (heartbeat newMid s) $ HM.toList nextIndices

-- For a given other server, get the AE they need
heartbeat :: String -> Server -> (String, Int) -> Message
heartbeat newMid s@Server{..} (dest, nextIndex) = message
  where commandsSend = if nextIndex == length slog then [] else drop nextIndex slog
        prevLogIndex = nextIndex - 1 --if nextIndex == 0 then nextIndex else nextIndex - 1
        prevLogTerm = if nextIndex <= 0 then 0 else cterm $ (slog!!(nextIndex - 1))
        rmessage = Just $ AE currentTerm sid prevLogIndex prevLogTerm commandsSend commitIndex
        message = Message sid dest sid RAFT (newMid ++ dest) Nothing Nothing rmessage

-- Leader executes the committed commands in its log and prepares the responses
-- to external clients these produce. Updates commitIndex
leaderExecute :: Server -> Server
leaderExecute s@Server{..}
  | commitIndex == toBeCommitted = s
  | otherwise = executedServer { commitIndex = toBeCommitted }
  where toBeCommitted = length slog - 1 -- minimum $ take majority $ reverse $ sort $ HM.elems matchIndices
        toBeExecuted = take (toBeCommitted - commitIndex) $ drop (commitIndex + 1) slog
        executedServer = execute s toBeExecuted

-- Run commands specified in the slog. Update the slog & add responses to sendMe
execute :: Server -> [Command] -> Server
execute s [] = s
execute s@Server{..} (Command{..}:cs)
  | ctype == CGET = execute s { sendMe = push (message (Just ckey) get) sendMe } cs
  | ctype == CPUT = execute s { sendMe = push (message (Just ckey) (Just cvalue)) sendMe, store = newStore } cs
    where get = HM.lookup ckey store
          newStore = HM.insert ckey cvalue store -- lazy eval ftw
          message k v = Message sid creator sid (if isNothing v then FAIL else OK) cmid k v Nothing

-- Given a time in the past, the current time, and a timeout (in ms) - have we exceeded this delta?
isExpired :: UTCTime -> UTCTime -> Int -> Bool
isExpired lastReceived now timeout = diff > timeout'
  where timeout' = 0.001 * realToFrac timeout
        diff = abs $ diffUTCTime now lastReceived

-- If the message is nothing and we've expired, transition to Candidate
-- If not, respond to the message
receiveMessage :: Server -> UTCTime -> Int -> Maybe Message -> Server
receiveMessage s@Server{..} time newTimeout Nothing
  | (sState == Follower || sState == Candidate) &&
    isExpired lastReceived time timeout = s { sState = Candidate,
                                              timeout = newTimeout,
                                              lastReceived = time,
                                              votes = HS.empty,
                                              currentTerm = currentTerm + 1 }
  | otherwise = s
-- receiveMessage _ _ _ m | trace (show m) False = undefined
receiveMessage s time _ (Just m@Message{..})
  | messType ==  GET = respondGet s m
  | messType == PUT = respondPut s m
  | messType == RAFT = respondRaft s' m -- update time on raft message
  | otherwise = s
      where s' = s { lastReceived = time }

-- If we aren't the leader, redirect to it. If we are, push this to our log.
respondGet :: Server -> Message -> Server
respondGet s@Server{..} m@Message{..}
  | sState == Leader = s { slog = push command slog }
  | otherwise = s { sendMe = push redirect sendMe }
    where command = Command CGET currentTerm src mid (fromJust key) ""
          redirect = Message sid src votedFor REDIRECT mid Nothing Nothing Nothing

-- If we aren't the leader, redirect. If we are, push to log
respondPut :: Server -> Message -> Server
respondPut s@Server{..} m@Message{..}
  | sState == Leader = s { slog = push command slog }
  | otherwise = s { sendMe = push redirect sendMe }
    where command = Command CPUT currentTerm src mid (fromJust key) (fromJust value)
          redirect = Message sid src votedFor REDIRECT mid Nothing Nothing Nothing

-- Respond to raft message - delegates based on current state
respondRaft :: Server -> Message -> Server
respondRaft s@Server{..} m@Message{..}
  | sState == Follower = respondFollower s m $ fromJust rmess
  | sState == Candidate = respondCandidate s m $ fromJust rmess
  | otherwise = respondLeader s m $ fromJust rmess

-- are the given params (last log index, last log term) as up to date as our current log?
upToDate :: [Command] -> Int -> Int -> Bool
upToDate [] _ _ = True
upToDate base lastLogTerm lastLogIndex = baseTI <= (lastLogTerm, lastLogIndex)
    where baseTI = (cterm $ last base, length base)


respondFollower :: Server -> Message -> RMessage -> Server
respondFollower s@Server{..} m@Message{..} r@RV{..}
  | term < currentTerm = s { sendMe = push (mRvr False) sendMe }
  | upToDate slog lastLogTerm lastLogIndex = s { sendMe = push (mRvr True) sendMe,
                                                 votedFor = candidateId,
                                                 currentTerm = term }
  | otherwise = s { sendMe = push (mRvr False) sendMe } -- should we update the term anyway?
    where mRvr isSuccess = Message sid src (if isSuccess then candidateId else votedFor) RAFT mid Nothing Nothing (Just $ RVR (if isSuccess then term else currentTerm) isSuccess)

respondFollower s@Server{..} m@Message{..} r@AE{..}
  | term < currentTerm = reject
  | (length slog <= prevLogIndex + 1) = inconsistent
  | prevLogIndex <= 0 = succeed
  | (cterm $ (slog!!prevLogIndex)) /= prevLogTerm = inconsistent { slog = deleteSlog }
  | otherwise = succeed
    where mReject = Message sid src votedFor RAFT mid Nothing Nothing $ Just $ AER currentTerm (-1) False
          reject = s { sendMe = push mReject sendMe }
          mIncons = Message sid src src RAFT mid Nothing Nothing $ Just $ AER term (-1) False
          inconsistent = s { votedFor = src, currentTerm = term, sendMe = push mIncons sendMe }
          deleteSlog = take prevLogIndex slog
          addSlog = slog ++ entries
          newCommitIndex = if leaderCommit > commitIndex then min leaderCommit (prevLogIndex + length entries) else commitIndex
          mSucceed = Message sid src src RAFT mid Nothing Nothing $ Just $ AER term (prevLogIndex + length entries) True
          succeed = s { slog = addSlog, commitIndex = newCommitIndex, currentTerm = term, sendMe = push mSucceed sendMe }

respondFollower s _ _ = error "wtf"

respondLeader :: Server -> Message -> RMessage -> Server
respondLeader s@Server{..} m@Message{..} r@AE{..}
  | term > currentTerm = s { sState = Follower,
                             currentTerm = term,
                             votedFor = src }
  | otherwise = s

respondLeader s@Server{..} m@Message{..} r@AER{..}
  | success == False = s { nextIndices = HM.adjust (subtract 1) src nextIndices }
  | success == True =  s { nextIndices = HM.insert src (lastIndex + 1) nextIndices,
                          matchIndices = HM.insert src lastIndex matchIndices }

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
