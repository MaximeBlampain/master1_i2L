{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module View where

import Model
import Lucid
import Data.Text as T
import Data.Text.Lazy as L

viewIndexPage :: [Rider] -> L.Text
viewIndexPage riders = mkPage "page1" $ do
    mapM_ mkRider riders

mkRider :: Rider -> Html ()
mkRider r = div_ $ do
    h2_ $ toHtml $ _name r
    mapM_ mkImg (_imgs r)

mkImg :: String -> Html ()
mkImg i = div_ $ do
    img_ [src_ (T.pack i)]
    p_ $ toHtml $ i


style :: T.Text
style = "styles.css"

mkPage :: L.Text -> Html () -> L.Text
mkPage myTitle myHtml = renderText $ do
    doctype_
    html_ $ do
        head_ $ do
            meta_ [charset_ "utf-8"]
            meta_ [ name_ "viewport"
                  , content_ "width=device-width,initial-scale=1,shrink-to-fit=no"]
            link_ [ rel_ "stylesheet", href_ style]
            title_ $ toHtml myTitle
        body_ $ div_ [class_ "container"] myHtml