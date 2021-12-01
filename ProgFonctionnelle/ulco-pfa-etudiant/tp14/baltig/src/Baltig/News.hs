{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module Baltig.News where

import Data.Aeson
import Database.Selda

data News = News
    { news_id     :: ID News
    , news_title  :: Text
    , news_date   :: Text
    , news_body   :: Text
    } deriving (Eq, Generic, Show)

instance SqlRow News

news_table :: Table News
news_table = table "news" [#news_id :- autoPrimary]

instance FromJSON News where
    parseJSON = withObject "News" $ \v -> News
        <$> (toId <$> v .: "id")
        <*> v .: "title"
        <*> v .: "date"
        <*> v .: "body"

instance ToJSON News where
    toJSON (News i t d b) = object
        [ "id" .= fromId i
        , "title" .= t
        , "date" .= d
        , "body" .= b
        ]

