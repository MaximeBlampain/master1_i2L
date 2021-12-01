{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Database.SQLite.Simple
import GHC.Generics (Generic)

data Titre = Titre 
    { _artist    :: Text
    , _title :: Text
    } deriving (Generic, Show)

instance FromRow Titre where
    fromRow = Titre <$> field <*> field 

data Artist = Artist 
    { _artist_id    :: Int
    , _artist_name :: Text
    } deriving (Generic, Show)

instance FromRow Artist where
    fromRow = Artist <$> field <*> field 

selectTitre :: Connection -> IO [Titre]
selectTitre conn = query_ conn 
    "SELECT artist_name, title_name FROM title \
    \INNER JOIN artist ON title_artist = artist_id"

selectAllArtist:: Connection -> IO [Artist]
selectAllArtist conn = query_ conn "SELECT * FROM artist"

main :: IO ()
main = do withConnection "music.db" selectAllArtist >>= mapM_ print
          withConnection "music.db" selectTitre >>= mapM_ print 