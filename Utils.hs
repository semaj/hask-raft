module Utils where

import Message

import qualified Data.HashMap.Strict as HM

import Data.Time.Clock
import Data.Hashable
import Debug.Trace

majority :: Int
majority = 2 -- because 1 is always implied (1 + 2)
           
timeoutRange :: (Int, Int)
timeoutRange = (400, 700) -- ms

sendCooldown = 0.037

cooledOff :: UTCTime -> UTCTime -> Bool
cooledOff now t = diff > sendCooldown
    where diff = abs $ diffUTCTime now t

timedOut :: UTCTime -> UTCTime -> Int -> Bool
timedOut clock now timeout = diff > timeout'
  where timeout' = 0.001 * realToFrac timeout
        diff = abs $ diffUTCTime now clock

ts :: (Show a) => UTCTime -> a -> String
ts now a = (show now) ++ " : " ++ (show a)

push :: a -> [a] -> [a]
push a as = as ++ [a]

getLastLogIndex :: [Command] -> Int
getLastLogIndex slog = length slog - 1 -- if length slog == 0 then (-1) else length slog - 1

getLastLogTerm :: [Command] -> Int
getLastLogTerm slog = if length slog == 0 then (-1) else cterm $ last slog

zipAddAllM :: [String] -> [Message] -> HM.HashMap String Message -> HM.HashMap String Message
zipAddAllM as bs hm
  | length as /= length bs = error $ "zipAddAllM: list lengths are not the same size " ++ (show as) ++ " : " ++ (show bs)
  | otherwise = HM.union (HM.fromList $ zip as bs) hm

zipAddAllT :: [String] -> [UTCTime] -> HM.HashMap String UTCTime -> HM.HashMap String UTCTime
zipAddAllT as bs hm
  | length as /= length bs = error $ "zipAddAllT: list lengths are not the same size " ++ (show as) ++ " : " ++ (show bs)
  | otherwise = HM.union (HM.fromList $ zip as bs) hm

getNeedResending :: UTCTime -> HM.HashMap a UTCTime -> [a]
getNeedResending now timeQ = HM.keys $ HM.filter (cooledOff now) timeQ

getPrevLogIndex :: Int -> Int
getPrevLogIndex nextIndex = nextIndex - 1

getPrevLogTerm :: [Command] -> Int -> Int
getPrevLogTerm [] _ = (-1)
getPrevLogTerm slog nextIndex
  | nextIndex > length slog = error $ "getPrevLogTerm: nextIndex greater than slog length " ++ (show nextIndex) ++ " : " ++ (show $ length slog)
  | nextIndex <= 0 = (-1)
  | otherwise = cterm $ (slog!!(nextIndex - 1))

getNextCommands :: [Command] -> Int -> [Command]
getNextCommands [] _ = []
getNextCommands slog nextIndex
  | nextIndex > length slog = error $ "getNextCommands: nextIndex greater than slength " ++ (show nextIndex) ++ " : " ++ (show $ length slog)
  | nextIndex == length slog = []
  | otherwise = take 15 $ drop nextIndex slog

upToDate :: [Command] -> Int -> Int -> Bool
upToDate [] _ _ = True
upToDate base lastLogTerm lastLogIndex
  | lastLogTerm >= myLastLogTerm && lastLogIndex >= myLastLogIndex = True
  | lastLogIndex < myLastLogIndex = trace ("theirs: " ++ (show lastLogIndex) ++ ", mine: " ++ (show myLastLogIndex)) False
  | otherwise = False
    where myLastLogTerm = cterm $ last base
          myLastLogIndex = length base - 1

getNewCommitIndex :: Int -> Int -> Int -> Int -> Int
getNewCommitIndex leaderCommit commitIndex prevLogIndex lengthEntries
  | leaderCommit > commitIndex = min leaderCommit (prevLogIndex + lengthEntries)
  | otherwise = commitIndex

cleanSlog :: [Command] -> Int -> [Command]
cleanSlog slog prevLogIndex = take prevLogIndex slog


