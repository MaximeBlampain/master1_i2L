{- LANGUAG-}

import Datanase.Selda
import Database.Selda.SQLite

data Artist = Artist
    { artist_id :: ID Artist
    , artist_name :: Text
    } deriving (Generic, Show)

instance SqlRow Artist

artists :: Table Artist
artists = table "artist" [#artist_id :- autoPrimary]

selectAllArtists :: SeldaT SQLite IO [Artist]
selectAllArtists = query $ select artist_table

main :: IO ()
main = withSQLite "music.db" selectAllArtists >>= mapM_ print