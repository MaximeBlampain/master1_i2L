{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Site where

import Data.Aeson
import GHC.Generics ( Generic )
import Lucid

data Site = Site
    {imgs   :: [String]
    , url   :: String
    } deriving (Generic, Show)

instance FromJSON Site

loadSites :: String -> IO (Either String [Site])
loadSites = eitherDecodeFileStrict