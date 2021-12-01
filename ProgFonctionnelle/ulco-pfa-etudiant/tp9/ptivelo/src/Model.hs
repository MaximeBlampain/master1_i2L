{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Model where

import Data.Aeson
import GHC.Generics
data Rider = Rider
    {
        _name :: String
    ,   _imgs :: [String]
    } deriving (Generic, Show)

instance ToJSON Rider