{-# LANGUAGE DeriveGeneric #-}

module Repo where

import Data.Aeson
import Data.Text
import GHC.Generics
import User

data Repo = Repo
    { id :: Int
    , name :: Text
    , full_name :: Text
    , owner :: User
    } deriving (Eq, Generic, Show)

instance ToJSON Repo
instance FromJSON Repo

