{-# LANGUAGE OverloadedStrings #-}

import Lucid
import Control.Monad.IO.Class (liftIO)
import Database.SQLite.Simple
import Web.Scotty

import Databases
import View
import Model

main :: IO ()
main = scotty 3000 $ do
    get "/" $ html $ indexPage
    get "/messages" $ do
        res <- liftIO $ withConnection "thread.db" dbSelectAllMessages
        
    {--get "/alldata" $ do
        datas <- liftIO $ withConnection "thread.db" Databases.dbSelectAllMessages
        html $ renderText $ (alldata menu datas)
    get "/allthreads" $ do
    html $ renderText (allthreads menu)--}

menu :: Html ()
menu = do
    doctype_
    html_ $ do
        head_ $ do
            meta_ [charset_ "utf-8"]
            title_ "ULCO - Index"
        body_ $ do
            h1_ "Bienvenue sur le forum de l'ULCO"
            ul_ $ do
                li_ $ do a_ [href_ "/alldata"] "alldata"
                li_ $ do a_ [href_ "/allthreads"] "allthreads"
getIndex :: Html () -> Html ()
getIndex h = do
    h
    div_ $ do
        h5_ "This is indexPage"
    

alldata :: Html () -> [Message] -> Html ()
alldata h datas = do
    h
    div_ [id_ "all-data"] $ do
        ul_ $ mapM_ viewAll datas

viewAll :: Message -> Html()
viewAll (Message user message thread) = li_ $ do
    span_ [class_ "thread-title"] toHtml thread
    br_
    p_ [class_ "message"]  
        toHtml user 
        " : "
        toHtml message

allthreads :: Html () -> Html ()
allthreads h = do
    h
    div_ [id_ "display-data"] $ do
        ul_ $ do 
            li_ $ do 
                span_ "COUCOU"