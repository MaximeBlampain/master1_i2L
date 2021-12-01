{-# LANGUAGE DeriveGeneric #-}

module User where

import Data.Aeson
import Data.Text
import GHC.Generics

data User = User 
    { id :: Int
    , login :: Text 
    } deriving (Eq, Generic, Show)

instance ToJSON User
instance FromJSON User

