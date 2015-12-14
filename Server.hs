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

-- (!)s just force a strict datatype, as Haskell is a lazy language by default
data Server = Server {
  sState :: !ServerState,
  sid :: !String,                      -- ID
  others :: ![String],                 -- The other server IDs
  store :: HM.HashMap String String,   -- The hashmap of keys & values
  sendMe :: ![Message],                -- Messages to send this round
  messQ :: HM.HashMap String Message,  -- Server -> Next message to send (not heartbeat!)
  timeQ :: HM.HashMap String UTCTime,  -- Server -> Last time we sent a message (not heartbeat!)
  pendingQ :: [Message],               -- Followers/Candidates store this so when a leader is elected, flush
  
  currentTerm :: Int,
  votedFor :: !String,                 -- Our leader
  slog :: ![Command],                  -- Log of commands to execute
  
  commitIndex :: Int,
  lastApplied :: Int,
  
  nextIndices :: HM.HashMap String Int,
  matchIndices :: HM.HashMap String Int,
  
  votes :: HS.HashSet String,          -- Set filled with servers that voted for us
  
  timeout :: Int,                      -- When we will timeout to candidate state (ms)
  lastHB :: UTCTime,                   -- Last time we sent a heartbeat
  clock :: UTCTime                     -- last time we received a raft message OR started an election
} deriving (Show)

initServer :: String -> [String] -> UTCTime -> Int -> Server
initServer myID otherIDs time timeout = Server { sid = myID,
                                                 others = otherIDs,
                                                 sState = Follower,
                                                 store = HM.empty,
                                                 messQ = HM.empty,
                                                 timeQ = HM.fromList $ map (\x -> (x, time)) otherIDs,
                                                 pendingQ = [],
                                                 sendMe = [],
                                                 currentTerm = 0,
                                                 votedFor = "FFFF",
                                                 slog = [],
                                                 commitIndex = -1,
                                                 lastApplied = -1,
                                                 nextIndices = HM.fromList $ map (\x -> (x, 0)) otherIDs,
                                                 matchIndices = HM.fromList $ map (\x -> (x, (-1))) otherIDs,
                                                 lastHB = time,
                                                 votes = HS.empty,
                                                 timeout = timeout,
                                                 clock = time }


-- The main function for transitioning servers
step :: String -> UTCTime -> Server -> Server
step newMid now s@Server{..}
  | sState == Follower = followerExecute s
  | sState == Candidate = checkVotes $ serverSend now $ candidatePrepare newMid $ s
  | sState == Leader = sendHBs now $ serverSend now $ leaderPrepare newMid $ leaderExecute s

-- Check if the candidate has enough votes. If so, transition to Leader.
checkVotes :: Server -> Server
checkVotes s@Server{..}
  -- | HS.size votes >= majority = trace (sid ++ " to lead w/ " ++ (show $ HS.size votes) ++ " votes, term " ++ (show currentTerm)) $
  | HS.size votes >= majority = s { sState = Leader,
                                   votedFor = sid,
                                   messQ = HM.empty,
                                   timeQ = HM.fromList $ map (\x -> (x, clock)) others,
                                   nextIndices = HM.map (const $ commitIndex + 1) nextIndices,
                                   matchIndices = HM.map (const commitIndex) matchIndices,
                                   sendMe = map (\srvr -> leaderAE
                                                          commitIndex
                                                          currentTerm
                                                          sid
                                                          ("init" ++ srvr)
                                                          slog
                                                          (srvr, (commitIndex + 1))) others,
                                   votes = HS.empty }
  | otherwise = s

-- Produce a RequestVote for a given Candidate's information
candidateRV :: Int -> String -> String -> [Command] -> String -> Message
candidateRV currentTerm src baseMid slog dst = Message src dst "FFFF" RAFT (baseMid ++ dst) Nothing Nothing rv
  where lastLogIndex = getLastLogIndex slog
        lastLogTerm = getLastLogTerm slog
        rv = Just $ RV currentTerm src lastLogIndex lastLogTerm

-- Add missing messages (RVs) to our message Q so that they're ready
candidatePrepare :: String -> Server -> Server
candidatePrepare newMid s@Server{..} = s { messQ = newMessQ }
  where recipients = filter (\ srvr -> (not $ HS.member srvr votes) && (not $ HM.member srvr messQ)) others
        newRVs = map (candidateRV currentTerm sid newMid slog) recipients
        newMessQ = zipAddAllM recipients newRVs messQ

-- Given a server, send all the things in our messQ if they've timed out in the timeQ
serverSend :: UTCTime -> Server -> Server
serverSend now s@Server{..} = s { sendMe = sendMe ++ resendMessages, timeQ = newTimeQ }
  where resendMe = getNeedResending now timeQ
        resendMessages = catMaybes $ map (\ srvr -> HM.lookup srvr messQ) resendMe
        newTimeQ = zipAddAllT resendMe (replicate (length resendMe) now) timeQ

-- If enough time has elapsed, send blank, special heartbeat messages
sendHBs :: UTCTime -> Server -> Server
sendHBs now s@Server{..}
  | timedOut lastHB now heartbeatRate = s { sendMe = push hb sendMe, lastHB = now }
  | otherwise = s
  where ae = Just $ AE (-5) sid (-5) (-5) [] (-5)
        hb = Message sid "FFFF" sid RAFT "HEARTBEAT" Nothing Nothing ae

-- Prepare AEs that need to be send, constructing appropriate list of commands
leaderPrepare :: String -> Server -> Server
leaderPrepare newMid s@Server{..} = s { messQ = filteredMessQ }
    where newAEs = map (\ srvr -> leaderAE commitIndex currentTerm sid newMid slog (srvr, (HM.!) nextIndices srvr)) others
          newMessQ = zipAddAllM others newAEs messQ
          filteredMessQ = HM.filter noHeartbeat newMessQ

-- Returns false if the entries we would send are empty
noHeartbeat :: Message -> Bool
noHeartbeat (Message _ _ _ _ _ _ _ (Just (AE _ _ _ _ [] _))) = False
noHeartbeat _ = True

leaderAE :: Int -> Int -> String -> String -> [Command] -> (String, Int) -> Message
leaderAE commitIndex currentTerm src baseMid slog (dst, nextIndex) = message
    where entries = getNextCommands slog nextIndex
          prevLogIndex = getPrevLogIndex nextIndex
          prevLogTerm = getPrevLogTerm slog nextIndex
          ae = Just $ AE currentTerm src prevLogIndex prevLogTerm entries commitIndex
          message = Message src dst src RAFT (baseMid ++ dst) Nothing Nothing ae

-- Execute the commands in our slog that have been replicated sufficiently
leaderExecute :: Server -> Server
leaderExecute s@Server{..}
  | commitIndex == toBeCommitted = s
  | otherwise = executedServer { commitIndex = toBeCommitted, lastApplied = toBeCommitted }
  where toBeCommitted = minimum $ take majority $ reverse $ sort $ HM.elems matchIndices -- (length slog ) - 1
        toBeExecuted = take (toBeCommitted - commitIndex) $ drop (commitIndex + 1) slog
        executedServer = execute s toBeExecuted

-- Execute the commands in our slog if the leader has told us they've been committed
followerExecute :: Server -> Server
followerExecute s@Server{..}
  | commitIndex == lastApplied = s
  | otherwise = executed { lastApplied = commitIndex }
    where toBeExecuted = drop (lastApplied + 1) $ take (commitIndex + 1) slog
          executed = (execute s toBeExecuted) { sendMe = sendMe }

-- Actually run the commands on our Store hashmap
execute :: Server -> [Command] -> Server
execute s [] = s
execute s@Server{..} (Command{..}:cs)
  | ctype == CGET = execute s { sendMe = push (message (Just ckey) get) sendMe } cs
  | ctype == CPUT = execute s { sendMe = push (message (Just ckey) (Just cvalue)) sendMe, store = newStore } cs
    where get = HM.lookup ckey store
          newStore = HM.insert ckey cvalue store 
          message k v = Message sid creator sid (if isNothing v then FAIL else OK) cmid k v Nothing

-- If we aren't a leader and we've timed out, transition to Candidate state
maybeToCandidate :: UTCTime -> Int -> Server -> Server
maybeToCandidate now newTimeout s
  | (sState s) == Leader = s
  -- | timedOut (clock s) now (timeout s) = trace ((sid s) ++ " timed out (current leader " ++ (votedFor s) ++ "), moving to term " ++ (show $ currentTerm s + 1)) candidate
  | timedOut (clock s) now (timeout s) = candidate
  | otherwise = s
    where candidate =  s { sState = Candidate,
                           timeout = newTimeout,
                           messQ = HM.empty,
                           sendMe = [],
                           -- votedFor = "FFFF", not sure this is necessary yet TODO
                           clock = now,
                           votes = HS.empty,
                           currentTerm = (currentTerm s) + 1 }

-- If the message is nothing and we've expired, transition to Candidate
-- If not, respond to the message
receiveMessage :: Server -> UTCTime -> Int -> Maybe Message -> Server
receiveMessage s time newTimeout Nothing = maybeToCandidate time newTimeout s
receiveMessage s time newTimeout (Just m@Message{..})
  | messType ==  GET = maybeToCandidate time newTimeout $ respondGet s m
  | messType == PUT = maybeToCandidate time newTimeout $ respondPut s m
  | messType == RAFT = maybeToCandidate time newTimeout $ respondRaft time s m
  | otherwise = maybeToCandidate time newTimeout s

clearPendingQ :: Server -> Server
clearPendingQ s@Server{..}
  | length pendingQ == 0 = s
  | otherwise = clearPendingQ $ responded { pendingQ = tail pendingQ }
  where pending = head pendingQ
        responded = if (messType pending) == GET then respondGet s pending else respondPut s pending

-- If we aren't the leader, redirect to it. If we are, push this to our log.
respondGet :: Server -> Message -> Server
respondGet s@Server{..} m@Message{..}
  | sState == Leader = s { slog = push command slog }
  | sState == Candidate = s { pendingQ = push m pendingQ }
  | otherwise = s { sendMe = push redirect sendMe }
    where command = Command CGET currentTerm src mid (fromJust key) ""
          redirect = Message sid src votedFor REDIRECT mid Nothing Nothing Nothing

-- If we aren't the leader, redirect. If we are, push to log
respondPut :: Server -> Message -> Server
respondPut s@Server{..} m@Message{..}
  | sState == Leader = s { slog = push command slog }
  | sState == Candidate = s { pendingQ = push m pendingQ }
  | otherwise = s { sendMe = push redirect sendMe }
    where command = Command CPUT currentTerm src mid (fromJust key) (fromJust value)
          redirect = Message sid src votedFor REDIRECT mid Nothing Nothing Nothing

-- Respond to raft message - delegates based on current state
respondRaft :: UTCTime -> Server -> Message -> Server
respondRaft now s@Server{..} m@Message{..}
  | sState == Follower = respondFollower now s m $ fromJust rmess
  | sState == Candidate = respondCandidate s m $ fromJust rmess
  | otherwise = respondLeader s m $ fromJust rmess

-- Get a RequestVote Response, given a follower's information
followerRVR :: String -> Int -> String -> String -> Int -> String -> Bool -> Message
followerRVR candidate term mid votedFor currentTerm src success = message
    where realTerm = if success then term else currentTerm
          realLeader = if success then candidate else votedFor
          rvr = Just $ RVR realTerm success
          message = Message src candidate realLeader RAFT mid Nothing Nothing rvr

-- Respond to a message. (as a follower)
respondFollower :: UTCTime -> Server -> Message -> RMessage -> Server
respondFollower now s@Server{..} m@Message{..} r@RV{..} -- Respond to RV
  -- | term < currentTerm = trace (sid ++ " rejecting " ++ candidateId ++ " for term. mine: " ++ (show currentTerm) ++ ", theirs: " ++ (show term)) reject
  | term < currentTerm = reject
  -- | upToDate slog lastLogTerm lastLogIndex = trace (sid ++ " granting " ++ candidateId ++ " for term " ++ show term) grant
  | upToDate slog lastLogTerm lastLogIndex = grant
  -- | otherwise = trace (sid ++ " rejecting " ++ candidateId ++ " for up-to-dateness, their term: " ++ (show term) ++ ", my term: " ++ show currentTerm ++ ", am I timed out? " ++ (show $ clock)) reject { currentTerm = term } -- should we update the term anyway?
  | otherwise = reject { currentTerm = term } -- should we update the term anyway?
    where baseMessage = followerRVR candidateId term mid votedFor currentTerm sid  -- needs success (curried)
          grant = s { sendMe = push (baseMessage True) sendMe, votedFor = candidateId, currentTerm = term }
          reject = s { sendMe = push (baseMessage False) sendMe }

respondFollower now s@Server{..} m@Message{..} r@AE{..} -- Respond to AE
  | mid == "HEARTBEAT" = s { clock = now }
  | term < currentTerm = reject
  | prevLogIndex <= 0 = succeed
  | (length slog - 1 < prevLogIndex) = inconsistent
  | (cterm $ (slog!!prevLogIndex)) /= prevLogTerm = inconsistent { slog = deleteSlog }
  | otherwise = succeed
    where mReject = Message sid src votedFor RAFT mid Nothing Nothing $ Just $ AER currentTerm (-1) False
          reject = s { sendMe = push mReject sendMe, clock = now }
          mIncons = Message sid src src RAFT mid Nothing Nothing $ Just $ AER term (-1) False
          inconsistent = s { votedFor = src, currentTerm = term, sendMe = push mIncons sendMe, clock = now }
          deleteSlog = cleanSlog slog prevLogIndex
          addSlog = union slog entries
          newCommitIndex = getNewCommitIndex leaderCommit commitIndex prevLogIndex (length entries)
          mSucceed = Message sid src src RAFT mid Nothing Nothing $ Just $ AER term (length addSlog - 1) True
          succeed = s { slog = addSlog,
                        commitIndex = newCommitIndex,
                        currentTerm = term,
                        sendMe = push mSucceed sendMe,
                        clock = now }

respondFollower _ s _ r = s

-- Respond to messages as a Leader
respondLeader :: Server -> Message -> RMessage -> Server
respondLeader s@Server{..} m@Message{..} r@AE{..} -- Respond to another AE
  | term > currentTerm = s { sState = Follower, currentTerm = term, votedFor = src }
  | otherwise = s

respondLeader s@Server{..} m@Message{..} r@AER{..} -- Respond to AE responses
  | success == False = s { nextIndices = HM.adjust (\x -> if x <= 0 then 0 else x - 1) src nextIndices,
                          messQ = newMessQ }
  | success == True =  s { nextIndices = HM.insert src newNextIndex nextIndices,
                          matchIndices = HM.insert src newMatchIndex matchIndices,
                          messQ = newMessQ }
    where newMessQ = HM.delete src messQ
          newNextIndex = if lastIndex >= length slog then length slog - 1 else lastIndex + 1
          newMatchIndex = if lastIndex >= length slog then length slog - 1 else lastIndex

respondLeader s@Server{..} m@Message{..} _ = s

-- Respond to messages as a Candidate
respondCandidate :: Server -> Message -> RMessage -> Server
respondCandidate s@Server{..} m@Message{..} r@RVR{..} -- respond to RequestVote responses
  | voteGranted == True = s { votes = HS.insert src votes, messQ = HM.delete src messQ }
  | otherwise = s
respondCandidate s@Server{..} m@Message{..} r@AE{..} -- respond to AE responses
  | term >= currentTerm = clearPendingQ $ s { sState = Follower, currentTerm = term, votedFor = src }
  | otherwise = s
respondCandidate s@Server{..} m@Message{..} r@RV{..} -- respond to other RVs
  -- | term > currentTerm && upToDate slog lastLogTerm lastLogIndex = trace (sid ++ " Cgranting " ++ candidateId ++ " for term " ++ show term ++ ", my term was " ++ (show currentTerm)) grant { sState = Follower }
  | term > currentTerm && upToDate slog lastLogTerm lastLogIndex = grant { sState = Follower }
  | otherwise =  reject 
    where baseMessage = followerRVR candidateId term mid votedFor currentTerm sid  -- needs success (curried)
          grant = s { sendMe = push (baseMessage True) sendMe,
                      votedFor = candidateId,
                      currentTerm = term,
                      votes = HS.empty }
          reject = s { sendMe = push (baseMessage False) sendMe }
respondCandidate s _ r = s 
