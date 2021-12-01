{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as L
import Lucid
import Network.Wai.Middleware.Gzip (gzip, def, gzipFiles,  GzipFiles(..))
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import System.Environment (lookupEnv)
import Web.Scotty

main :: IO ()
main = do
    port <- read . fromMaybe "3000" <$> lookupEnv "PORT"
    scotty port $ do
        middleware logStdout
        middleware $ gzip def { gzipFiles = GzipCompress }
        middleware $ staticPolicy $ addBase "static"
        get "/" $ html $ indexPage

indexPage :: L.Text
indexPage = renderText $ do
    doctype_
    html_ $ do
        head_ $ do
            meta_ [charset_ "utf-8"]
            link_ [rel_ "stylesheet", href_ "styles.css"]
            with 
                (script_ mempty) 
                [src_ "all.js", async_ mempty, defer_ mempty] 
            title_ "img-full"
        body_ mempty

