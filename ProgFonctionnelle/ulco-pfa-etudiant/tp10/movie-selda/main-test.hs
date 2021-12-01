{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
import Database.Selda (toId)
import Database.Selda.Backend (runSeldaT)
import Database.Selda.SQLite (sqliteOpen, seldaClose)
import System.Directory (doesFileExist)

import Movie

dbFilename :: String
dbFilename = "movie.db"

main :: IO ()
main = do

    dbExists <- doesFileExist dbFilename
    conn <- sqliteOpen dbFilename
    when (not dbExists) $ runSeldaT dbInit conn

    -- TODO

    seldaClose conn

