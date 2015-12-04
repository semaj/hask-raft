{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Message where
import Data.Aeson
import Data.Char
import Data.Maybe
import GHC.Generics

timeoutRange :: (Int, Int)
timeoutRange = (150, 300) -- ms

type Command = (String, String)

data MessType = GET | PUT | OK | FAIL | REDIRECT | RAFT deriving (Read)

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
} deriving (Show)

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
  source :: String,
  dest :: String,
  leaderId :: !String,
  prevLogIndex :: Int,
  entries :: [Command],
  leadercommit :: Int
} | AER {
  term :: Int,
  source :: String,
  dest :: String,
  success :: Bool
} | RV {
  term :: Int,
  source :: String,
  dest :: String,
  candidateId :: !String,
  lastLogIndex :: Int,
  lastLogTerm :: Int
} | RVR {
  term :: Int,
  source :: String,
  dest :: String,
  voteGranted :: Bool
} deriving (Generic, Show)

instance ToJSON RMessage
instance FromJSON RMessage







