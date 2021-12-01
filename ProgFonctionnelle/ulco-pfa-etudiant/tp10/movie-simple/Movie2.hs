{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Movie2 where

import Data.Text (Text)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow (FromRow, fromRow, field)
import GHC.Generics (Generic)

data Prod = Prod
    { _movie :: Text
    , _year :: Int
    , _person :: Text
    , _role :: Text
    } deriving (Generic, Show)

instance FromRow Prod where
    fromRow = Prod <$> field <*> field <*> field <*> field

dbSelectAllProds :: Connection -> IO [Prod]
dbSelectAllProds conn =
    query_ conn "SELECT movie_title, movie_year, person_name, role_name \
                \FROM prod \
                \INNER JOIN movie ON prod_movie = movie_id \
                \INNER JOIN person ON prod_person = person_id \
                \INNER JOIN role ON prod_role = role_id"

