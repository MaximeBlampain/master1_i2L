{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module Movie where

import Control.Monad.IO.Class (liftIO)
import Database.Selda
import Database.Selda.SQLite

----------------------------------------------------------------------
-- Movie
----------------------------------------------------------------------

data Movie = Movie
  { movie_id :: ID Movie
  , movie_title :: Text
  , movie_year :: Int
  } deriving (Generic, Show)

instance SqlRow Movie

movie_table :: Table Movie
movie_table = table "movie" [#movie_id :- autoPrimary]

----------------------------------------------------------------------
-- Person
----------------------------------------------------------------------

data Person = Person
  { person_id :: ID Person
  , person_name :: Text
  } deriving (Generic, Show)

instance SqlRow Person

person_table :: Table Person
person_table = table "person" [#person_id :- autoPrimary]

----------------------------------------------------------------------
-- queries
----------------------------------------------------------------------

dbInit :: SeldaT SQLite IO ()
dbInit = do

    createTable movie_table
    tryInsert movie_table
        [ Movie def "Bernie" 1996
        , Movie def "Le Kid" 1921 
        , Movie def "Metropolis" 1927
        , Movie def "Citizen Kane" 1941 ]
        >>= liftIO . print

