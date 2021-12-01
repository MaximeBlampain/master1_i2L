{-# LANGUAGE OverloadedStrings #-}

import Lucid
import Control.Monad.IO.Class (liftIO)
import Database.SQLite.Simple
import TextShow
import Web.Scotty

import Movie2

main::IO()
main = scotty 3000 $
    get "/" $ do
        prods <- liftIO $ withConnection "movie.db" Movie2.dbSelectAllProds
        html $ renderText $ viewPage prods

viewPage :: [Prod] -> Html ()
viewPage prods = do
    doctype_
    html_ $ do
        head_ $ do
            meta_ [charset_ "utf-8"]
            title_ "movie-simple"
        body_ $ do
            h1_ "movie-simple"
            ul_ $ mapM_ viewProd prods

viewProd :: Prod -> Html ()
viewProd (Prod movie year person role) = li_ $ do
    toHtml movie
    " ("
    toHtml $ showt year
    "), "
    toHtml person
    " ("
    toHtml role
    ")"