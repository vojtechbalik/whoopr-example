{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Protolude
import Data.Time (UTCTime)
import Data.Aeson (ToJSON)
import Data.Aeson.TH (deriveToJSON, defaultOptions, deriveFromJSON)

type ID = Int

data User = User {
    id :: ID,
    credentials :: UserCredentials,
    last_seen :: UTCTime,
    account_created :: UTCTime
} deriving (Show, Generic)

data UserCredentials = UserCredentials {
    login :: Text,
    password :: Text
} deriving (Show, Generic)

instance Eq User where
    (==) = (==) `on` id

deriveToJSON defaultOptions 'UserCredentials
deriveFromJSON defaultOptions 'UserCredentials
deriveToJSON defaultOptions 'User