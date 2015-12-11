{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Server where

import Message
import Utils

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import Data.List
import Data.Aeson
import Data.Maybe
import Data.Time

import System.Random
import Debug.Trace

data ServerState = Follower | Candidate | Leader deriving (Show, Eq)

data Server = Server {
  sState :: !ServerState,
  sid :: !String,
  others :: ![String],
  store :: HM.HashMap String String,
  sendMe :: ![Message],
  messQ :: HM.HashMap String Message,
  timeQ :: HM.HashMap String UTCTime,
  -- Persistent state
  currentTerm :: Int,
  votedFor :: !String,
  slog :: ![Command],
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
  clock :: UTCTime -- last time we received a raft message OR started an election
} deriving (Show)

initServer :: String -> [String] -> UTCTime -> Int -> Server
initServer myID otherIDs time timeout = Server { sid = myID,
                                                 others = otherIDs,
                                                 sState = Follower,
                                                 store = HM.empty,
                                                 messQ = HM.empty,
                                                 timeQ = HM.fromList $ map (\x -> (x, time)) otherIDs,
                                                 sendMe = [],
                                                 currentTerm = 0,
                                                 votedFor = "FFFF",
                                                 slog = [],
                                                 commitIndex = -1,
                                                 lastApplied = -1,
                                                 nextIndices = HM.fromList $ map (\x -> (x, 0)) otherIDs,
                                                 matchIndices = HM.fromList $ map (\x -> (x, (-1))) otherIDs,
                                                 votes = HS.empty,
                                                 timeout = timeout,
                                                 clock = time }


step :: String -> UTCTime -> Server -> Server
step newMid now s@Server{..}
  | sState == Follower = followerExecute s
  | sState == Candidate = checkVotes $ serverSend now $ candidatePrepare newMid $ s
  | sState == Leader = serverSend now $ leaderPrepare newMid $ leaderExecute s

-- could convert Candidate -> Leader
checkVotes :: Server -> Server
checkVotes s@Server{..}
  | HS.size votes >= majority = s { sState = Leader,
                                   votedFor = sid,
                                   messQ = HM.empty,
                                   timeQ = HM.fromList $ map (\x -> (x, clock)) others,
                                   nextIndices = HM.map (const $ commitIndex) nextIndices,
                                   matchIndices = HM.map (const (commitIndex + 1)) matchIndices,
                                   votes = HS.empty }
  | otherwise = s

candidateRV :: Int -> String -> String -> [Command] -> String -> Message
candidateRV currentTerm src baseMid slog dst = Message src dst "FFFF" RAFT (baseMid ++ dst) Nothing Nothing rv
  where lastLogIndex = getLastLogIndex slog
        lastLogTerm = getLastLogTerm slog
        rv = Just $ RV currentTerm src lastLogIndex lastLogTerm

candidatePrepare :: String -> Server -> Server
candidatePrepare newMid s@Server{..} = s { messQ = newMessQ }
  where recipients = filter (\ srvr -> (not $ HS.member srvr votes) && (not $ HM.member srvr messQ)) others
        newRVs = map (candidateRV currentTerm sid newMid slog) recipients
        newMessQ = zipAddAllM recipients newRVs messQ

serverSend :: UTCTime -> Server -> Server
serverSend now s@Server{..} = s { sendMe = sendMe ++ resendMessages, timeQ = newTimeQ }
  where resendMe = getNeedResending now timeQ
        resendMessages = catMaybes $ map (\ srvr -> HM.lookup srvr messQ) resendMe
        newTimeQ = zipAddAllT resendMe (replicate (length resendMe) now) timeQ

leaderPrepare :: String -> Server -> Server
leaderPrepare newMid s@Server{..} = s { messQ = newMessQ }
    where recipients = filter (\ srvr -> not $ HM.member srvr messQ) others
          newAEs = map (\ srvr -> leaderAE commitIndex currentTerm sid newMid slog (srvr, (HM.!) nextIndices srvr)) others
          newMessQ = zipAddAllM others newAEs messQ

leaderAE :: Int -> Int -> String -> String -> [Command] -> (String, Int) -> Message
leaderAE commitIndex currentTerm src baseMid slog (dst, nextIndex) = message
    where entries = getNextCommands slog nextIndex
          prevLogIndex = getPrevLogIndex nextIndex
          prevLogTerm = getPrevLogTerm slog nextIndex
          ae = Just $ AE currentTerm src prevLogIndex prevLogTerm entries commitIndex
          message = Message src dst src RAFT (baseMid ++ dst) Nothing Nothing ae

-- Leader executes the committed commands in its log and prepares the responses
-- to external clients these produce. Updates commitIndex
leaderExecute :: Server -> Server
leaderExecute s@Server{..}
  -- | trace (show $ matchIndices) False = undefined
  -- | lastApplied >= length slog - 1 = s
  | commitIndex == toBeCommitted = s
  | otherwise = executedServer { commitIndex = toBeCommitted, lastApplied = toBeCommitted }
  where toBeCommitted = minimum $ take majority $ reverse $ sort $ HM.elems matchIndices -- (length slog ) - 1
        toBeExecuted = take (toBeCommitted - commitIndex) $ drop (commitIndex + 1) slog
        executedServer = execute s toBeExecuted

followerExecute :: Server -> Server
followerExecute s@Server{..}
  | commitIndex == lastApplied = s
  | otherwise = executed { lastApplied = commitIndex }
    where toBeExecuted = drop (lastApplied + 1) $ take (commitIndex + 1) slog
          executed = (execute s toBeExecuted) { sendMe = sendMe }

-- Run commands specified in the slog. Update the slog & add responses to sendMe
execute :: Server -> [Command] -> Server
execute s [] = s
execute s@Server{..} (Command{..}:cs)
  | ctype == CGET = execute s { sendMe = push (message (Just ckey) get) sendMe } cs
  | ctype == CPUT = execute s { sendMe = push (message (Just ckey) (Just cvalue)) sendMe, store = newStore } cs
    where get = HM.lookup ckey store
          newStore = HM.insert ckey cvalue store -- lazy eval ftw
          message k v = Message sid creator sid (if isNothing v then FAIL else OK) cmid k v Nothing

maybeToCandidate :: UTCTime -> Int -> Server -> Server
maybeToCandidate now newTimeout s
  | (sState s) == Leader = s
  | timedOut (clock s) now (timeout s) = candidate
  | otherwise = s
    where candidate =  s { sState = Candidate,
                           timeout = newTimeout,
                           clock = now,
                           votes = HS.empty,
                           currentTerm = (currentTerm s) + 1 }

-- If the message is nothing and we've expired, transition to Candidate
-- If not, respond to the message
receiveMessage :: Server -> UTCTime -> Int -> Maybe Message -> Server
receiveMessage s time newTimeout Nothing = maybeToCandidate time newTimeout s
receiveMessage s time _ (Just m@Message{..})
  | messType ==  GET = respondGet s m
  | messType == PUT = respondPut s m
  | messType == RAFT = respondRaft withNewClock m
  | otherwise = s
      where withNewClock = s { clock = time }

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

followerRVR :: String -> Int -> String -> String -> Int -> String -> Bool -> Message
followerRVR candidate term mid votedFor currentTerm src success = message
    where realTerm = if success then term else currentTerm
          realLeader = if success then candidate else votedFor
          rvr = Just $ RVR realTerm success
          message = Message src candidate realLeader RAFT mid Nothing Nothing rvr

respondFollower :: Server -> Message -> RMessage -> Server
respondFollower s@Server{..} m@Message{..} r@RV{..}
  | term < currentTerm = reject
  | upToDate slog lastLogTerm lastLogIndex = grant
  | otherwise = reject -- should we update the term anyway?
    where baseMessage = followerRVR candidateId term mid votedFor currentTerm sid  -- needs success (curried)
          grant = s { sendMe = push (baseMessage True) sendMe, votedFor = candidateId, currentTerm = term }
          reject = s { sendMe = push (baseMessage False) sendMe }

respondFollower s@Server{..} m@Message{..} r@AE{..}
  | term < currentTerm = reject
  | prevLogIndex <= 0 = succeed
  | (length slog - 1 < prevLogIndex) = inconsistent
  | (cterm $ (slog!!prevLogIndex)) /= prevLogTerm = inconsistent { slog = deleteSlog }
  | otherwise = succeed
    where mReject = Message sid src votedFor RAFT mid Nothing Nothing $ Just $ AER currentTerm (-1) False
          reject = s { sendMe = push mReject sendMe }
          mIncons = Message sid src src RAFT mid Nothing Nothing $ Just $ AER term (-1) False
          inconsistent = s { votedFor = src, currentTerm = term, sendMe = push mIncons sendMe }
          deleteSlog = cleanSlog slog prevLogIndex
          addSlog = slog ++ entries
          newCommitIndex = getNewCommitIndex leaderCommit commitIndex prevLogIndex (length entries)
          mSucceed = Message sid src src RAFT mid Nothing Nothing $ Just $ AER term (length addSlog - 1) True
          succeed = s { slog = addSlog, commitIndex = newCommitIndex, currentTerm = term, sendMe = push mSucceed sendMe }

respondFollower s _ r = s -- error $ "wtf " ++ (show r)

respondLeader :: Server -> Message -> RMessage -> Server
respondLeader s@Server{..} m@Message{..} r@AE{..}
  | term > currentTerm = s { sState = Follower, currentTerm = term, votedFor = src }
  | otherwise = s

respondLeader s@Server{..} m@Message{..} r@AER{..}
  | success == False = s { nextIndices = HM.adjust (\x -> if x <= 0 then 0 else x - 1) src nextIndices,
                          messQ = newMessQ }
  | success == True =  s { nextIndices = HM.insert src newNextIndex nextIndices,
                          matchIndices = HM.insert src newMatchIndex matchIndices,
                          messQ = newMessQ }
    where newMessQ = HM.delete src messQ
          newNextIndex = if lastIndex >= length slog then length slog - 1 else lastIndex + 1
          newMatchIndex = if lastIndex >= length slog then length slog - 1 else lastIndex

respondLeader s@Server{..} m@Message{..} _ = s

respondCandidate :: Server -> Message -> RMessage -> Server
respondCandidate s@Server{..} m@Message{..} r@RVR{..}
  | voteGranted == True = s { votes = HS.insert src votes, messQ = HM.delete src messQ }
  | otherwise = s
respondCandidate s@Server{..} m@Message{..} r@AE{..}
  | term >= currentTerm = s { sState = Follower, currentTerm = term, votedFor = src }
  | otherwise = s
respondCandidate s _ _ = s
