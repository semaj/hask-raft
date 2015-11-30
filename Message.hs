{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Message where
import Data.Aeson
import Data.Char
import Data.Maybe

data MessType = GET | PUT | OK | FAIL | REDIRECT deriving (Read)

instance Show MessType where
  show GET = "get"
  show PUT = "put"
  show OK = "ok"
  show FAIL = "fail"
  show REDIRECT = "redirect"

data Message = Message {
  src :: !String,
  dst :: !String,
  leader :: !String,
  messType :: !MessType,
  mid :: !String,
  key :: Maybe String,
  value :: Maybe String
} deriving (Show)

instance FromJSON Message where
  parseJSON = withObject "message" $ \o -> do
    src     <- o .: "src"
    dst     <- o .: "dst"
    leader  <- o .: "leader"
    rawType <- o .: "type"
    mid     <- o .: "MID"
    key     <- o .:? "key"
    value   <- o .:? "value" -- might not be there
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
    "value"  .= fromMaybe "" value ]



