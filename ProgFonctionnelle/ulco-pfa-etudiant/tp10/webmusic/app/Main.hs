{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Text.Read (readMaybe)
import Web.Scotty

import View

-- import DbSeldaPostgresql
import DbSeldaSqlite
-- import DbPostgresqlSimple
-- import DbSqliteSimple

main :: IO ()
main = scotty 3000 $ do
    middleware logStdout
    get "/" $ do
        pSelected <- param "selected" `rescue` (\_ -> return "Nothing")
        let selected = myIntToId <$> readMaybe pSelected
        liftIO $ print selected
        artists <- liftIO $ runDb selectAllArtists
        titles <- case selected of
            Just idArtist -> liftIO $ runDb (selectTitlesOfArtist idArtist)
            Nothing -> return []
        let model = Model selected artists titles
        html $ indexPage model

{-
main :: IO ()
main = do
    res1 <- runDb selectAllArtists
    mapM_ print res1
    res2 <- runDb $ selectTitlesOfArtist 0
    mapM_ print res2
-}

