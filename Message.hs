{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Message where
import Data.Aeson
import Data.Char
import Data.Maybe
import GHC.Generics
import Data.Time.Clock

timeoutRange :: (Int, Int)
timeoutRange = (500, 1000) -- ms

data SentMessage = SentMessage {
  message :: Maybe Message,
  sent :: UTCTime
} deriving (Show, Eq)

data CommandType = CGET | CPUT deriving (Show, Generic, Eq)
data Command = Command {
  ctype :: CommandType,
  cterm :: Int,
  creator :: !String,
  cmid :: !String,
  ckey :: !String,
  cvalue :: !String
} deriving (Show, Generic, Eq)

instance ToJSON Command
instance FromJSON Command
instance ToJSON CommandType
instance FromJSON CommandType

data MessType = GET | PUT | OK | FAIL | REDIRECT | RAFT deriving (Read, Eq)

instance Show MessType where
  show GET = "get"
  show PUT = "put"
  show OK = "ok"
  show FAIL = "fail"
  show REDIRECT = "redirect"
  show RAFT = "RAFT"

data Message = Message {
  src :: !String,
  dst :: !String,
  leader :: !String,
  messType :: !MessType,
  mid :: !String,
  key :: Maybe String,
  value :: Maybe String,
  rmess :: Maybe RMessage
} deriving (Show, Eq)

instance FromJSON Message where
  parseJSON = withObject "message" $ \o -> do
    src     <- o .: "src"
    dst     <- o .: "dst"
    leader  <- o .: "leader"
    rawType <- o .: "type"
    mid     <- o .: "MID"
    key     <- o .:? "key" -- might not be there
    value   <- o .:? "value" -- might not be there
    rmess   <- o .:? "rmess" -- will be here if messType == AE or RV or...
    let messType = (read $ map toUpper rawType) :: MessType
    return Message{..}

instance ToJSON Message where
  toJSON Message{..} = object [
    "src"    .= src,
    "dst"    .= dst,
    "leader" .= leader,
    "type"   .= (show messType),
    "MID"    .= mid,
    "key"    .= key, -- Aeson encodes these maybes as "null"
    "value"  .= value,
    "rmess"  .= rmess ]

data RMessage = AE {
  term :: Int,
  leaderId :: !String,
  prevLogIndex :: Int,
  prevLogTerm :: Int,
  entries :: [Command],
  leaderCommit :: Int
} | AER {
  term :: Int,
  lastIndex :: Int, -- ONLY USE THIS IF SUCCESS IS TRUE
  success :: Bool
} | RV {
  term :: Int,
  candidateId :: !String,
  lastLogIndex :: Int,
  lastLogTerm :: Int
} | RVR {
  term :: Int,
  voteGranted :: Bool
} deriving (Generic, Show, Eq)

instance ToJSON RMessage
instance FromJSON RMessage







