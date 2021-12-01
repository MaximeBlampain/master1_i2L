{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Lucid
import Miso (View, ToServerRoutes)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip (gzip, def, gzipFiles, GzipFiles(..))
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import System.Environment (lookupEnv)

import Baltig.Action
import Baltig.Model
import Baltig.News
import Baltig.Routes
import Baltig.View

-- import Data.Aeson
import qualified Database.Selda as SQL
import qualified Database.Selda.SQLite as SQL

----------------------------------------------------------------------
-- main 
----------------------------------------------------------------------

main :: IO ()
main = do
    port <- read . fromMaybe "3000" <$> lookupEnv "PORT"
    putStrLn $ "listening port " ++ show port ++ "..."
    run port 
        $ logStdoutDev 
        $ gzip def { gzipFiles = GzipCompress }
        $ serve (Proxy @ServerApi) serverApp

----------------------------------------------------------------------
-- server api and app
----------------------------------------------------------------------

type ServerApi
    =    ToServerRoutes ClientRoutes HtmlPage Action
    :<|> Raw

serverApp :: Server ServerApi
serverApp 
    =    (handleHome :<|> handleGitlab)
    :<|> serveDirectoryWebApp "static"

handleHome, handleGitlab :: Handler (HtmlPage (View Action))
handleHome = pure $ HtmlPage $ homeView $ createModel homeRoute
handleGitlab = pure $ HtmlPage $ gitlabView $ createModel gitlabRoute

----------------------------------------------------------------------
-- server-side rendering
----------------------------------------------------------------------

newtype HtmlPage a = HtmlPage a
    deriving (Show, Eq)

instance ToHtml a => ToHtml (HtmlPage a) where
    toHtmlRaw = toHtml
    toHtml (HtmlPage x) = doctypehtml_ $ do
        head_ $ do
            meta_ [charset_ "utf-8"]
            link_ [rel_ "stylesheet", href_ "styles.css"]
            meta_ [name_ "viewport"
                  ,content_ "width=device-width,initial-scale=1,shrink-to-fit=no"]
            with 
                (script_ mempty) 
                [src_ "all.js", async_ mempty, defer_ mempty] 
        body_ $ toHtml x

