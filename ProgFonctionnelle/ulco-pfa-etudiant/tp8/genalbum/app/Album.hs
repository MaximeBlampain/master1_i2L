{-# LANGUAGE OverloadedStrings #-}
module Album where

import Data.Text as T
import Lucid

import Site

mkPage :: [Site] -> Html()
mkPage _sites = do
    doctype_
    html_ $ do
        head_ $ meta_ [charset_ "utf8"]
        body_ $ do
            h1_ "Album"
            mapM_ mkSite sites


mkSite :: Site -> Html()
mkSite s = div_ $ do
    h2_ $ toHtml $ url s
    img_ [src_ (T.pack i)]
    --p_ $ toHtml $ head $ imgs s

            {-img_ [src_ "toto.png"]
            p_ $ do
                "this is"
                a_ [href_ "toto.png"] "a link"-}

genAlbum :: String
genAlbum filename sites = renderToFile filename (mkPage sites)