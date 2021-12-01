{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import Database.SQLite.Simple

selectAllArtists :: Connection -> IO[(Int,Text)]
selectAllArtists conn = query_ conn "SELECT * FROM artist"

selectAllTitles :: Connection -> IO[(Text, Text)]
selectAllTitles conn = query_ conn 
    "SELECT artist_name, title_name \
    \FROM title \
    \INNER JOIN artist ON title_artist = artist_id"

main :: IO ()
main = do
    conn <- open "music.db"

    res1 <- selectAllArtists conn
    mapM_ print res1

    res2 <-selectAllTitles conn
    mapM_ print res2

    close conn