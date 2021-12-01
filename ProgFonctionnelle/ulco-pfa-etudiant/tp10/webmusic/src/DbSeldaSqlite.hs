{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module DbSeldaSqlite where

import Database.Selda
import Database.Selda.SQLite

----------------------------------------------------------------------
-- Model & re-exports
----------------------------------------------------------------------

data Model = Model
    { modelSelected :: Maybe (ID Artist)
    , modelArtists  :: [Artist]
    , modelTitles   :: [Text]
    } 

myIntToId :: Int -> ID a
myIntToId = toId

myIdToInt :: ID a -> Int
myIdToInt = fromId

type IDArtist = ID Artist

databaseVersion :: String
databaseVersion = "Database.Selda.SQLite"

----------------------------------------------------------------------
-- Artist
----------------------------------------------------------------------

data Artist = Artist
  { artist_id :: ID Artist
  , artist_name :: Text
  } deriving Generic

instance SqlRow Artist

artist :: Table Artist
artist = table "artist" [#artist_id :- autoPrimary]

----------------------------------------------------------------------
-- Title
----------------------------------------------------------------------

data Title = Title
  { title_id :: ID Title
  , title_artist :: ID Artist
  , title_name :: Text
  } deriving Generic

instance SqlRow Title

title :: Table Title
title = table "title" [#title_id :- autoPrimary]

----------------------------------------------------------------------
-- queries
----------------------------------------------------------------------

runDb :: SeldaT SQLite IO a -> IO a
runDb = withSQLite "data/music.db"

selectAllArtists :: SeldaT SQLite IO [Artist]
selectAllArtists = query $ select artist

selectTitlesOfArtist :: ID Artist -> SeldaT SQLite IO [Text]
selectTitlesOfArtist idArtist = query $ do
    t <- select title
    restrict (t ! #title_artist .== literal idArtist)
    return (t ! #title_name)

