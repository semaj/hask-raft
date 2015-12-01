{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Message where
import Server
import Data.Aeson
import Data.Char
import Data.Maybe
import GHC.Generics

data MessType = GET | PUT | OK | FAIL | REDIRECT |
                AE | AEResponse | RV | RVResponse deriving (Read)

instance Show MessType where
  show GET = "get"
  show PUT = "put"
  show OK = "ok"
  show FAIL = "fail"
  show REDIRECT = "redirect"
  show AE = "AE"
  show AEResponse = "AEResponse"
  show RV = "RV"
  show RVResponse = "RVResponse"

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
    "key"    .= fromMaybe "" key,
    "value"  .= fromMaybe "" value,
    "rmess"  .= rmess ]

data RMessage = AEM {
  term :: Int,
  leaderId :: !String,
  prevLogIndex :: Int,
  entries :: [Command],
  leadercommit :: Int
  } | AER {
  term :: Int,
  success :: Bool
  } | RVM {
  term :: Int,
  candidateId :: !String,
  lastLogIndex :: Int,
  lastLogTerm :: Int
  } | RVR {
  term :: Int,
  voteGranted :: Bool
} deriving (Generic, Show)

instance ToJSON RMessage
instance FromJSON RMessage







