{-# LANGUAGE OverloadedStrings #-}

module View where

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Lucid
import TextShow

-- import DbSeldaPostgresql
import DbSeldaSqlite
-- import DbPostgresqlSimple
-- import DbSqliteSimple

paramViewport, paramBoostrap :: T.Text
paramViewport = "width=device-width, initial-scale=1, shrink-to-fit=no"
paramBoostrap = "https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css"

indexPage :: Model -> L.Text
indexPage (Model sel as ts) = renderText $ do
    doctype_
    html_ $ do
        head_ $ do
            meta_ [charset_ "utf-8"]
            meta_ [ name_ "viewport", content_ paramViewport]
            link_ [ rel_ "stylesheet", href_ paramBoostrap]
            title_ "webmusic"
        body_ $ div_ [class_ "container"] $ do
            h1_ "webmusic"
            hr_ []
            div_ [class_ "row"] $ div_ [class_ "container"] $ do
                h2_ "Artist"
                form_ [action_ "/"] $ 
                    select_ [name_ "selected", onchange_ "this.form.submit()"] $ do
                        option_ [name_ "Nothing"] ""
                        mapM_ (mkOption sel) as
            hr_ []
            div_ [class_ "row"] $ div_ [class_ "container"] $ do
                h2_ "Titles"
                case sel of
                    Nothing -> "no artist selected"
                    Just _ -> ul_ $ mapM_ (li_ . toHtml) ts
            hr_ []
            p_ [style_ "text-align:right"] $ toHtml databaseVersion 

mkOption :: Maybe IDArtist -> Artist -> Html ()
mkOption mSel (Artist i n)
    | mSel == (Just i) = option_ (pVal ++ [selected_ ""]) $ toHtml n
    | otherwise = option_ pVal $ toHtml n
    where pVal = [value_ (showt $ myIdToInt i)]

