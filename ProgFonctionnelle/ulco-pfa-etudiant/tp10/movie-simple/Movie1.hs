{-# LANGUAGE OverloadedStrings #-}

module Movie1 where

import Data.Text (Text)
import Database.SQLite.Simple

dbSelectAllMovies:: Connection -> IO [(Int,Text,Int)]
dbSelectAllMovies conn = query_ conn "SELECT * FROM movies"


dbSelectMoviesFromPersonId :: Connection -> Int -> IO [[Text]]
dbSelectMoviesFromPersonId conn  i=
    query conn "SELECT movie_title \
                \FROM prod \
                \ INNER JOIN movie ON prod_movie = movie_id \
                \WHERE prod_person = 7"