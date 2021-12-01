{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

import           Common

import           Data.Maybe (fromMaybe)
import           Data.Proxy (Proxy(..))
import qualified Lucid as L
import           Miso
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.Gzip (gzip, def, gzipFiles, GzipFiles(..))
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Servant
import           System.Environment (lookupEnv)

----------------------------------------------------------------------
-- main 
----------------------------------------------------------------------

main :: IO ()
main = do
    port <- read . fromMaybe "3000" <$> lookupEnv "PORT"
    putStrLn $ "listening port " ++ show port ++ "..."
    run port 
        $ logStdout 
        $ gzip def { gzipFiles = GzipCompress }
        $ serve (Proxy @ServerApi) serverApp

----------------------------------------------------------------------
-- server api and app
----------------------------------------------------------------------

type ServerApi
    =    HelloApi
    :<|> ToServerRoutes ClientRoutes HtmlPage Action
    :<|> StaticApi

type StaticApi = Raw

serverApp :: Server ServerApi
serverApp 
    =    (pure "Hello")
    :<|> (handleHome)
    :<|> serveDirectoryWebApp "static"

handleHome :: Handler (HtmlPage (View Action))
handleHome = pure $ HtmlPage $ homeView initModel

----------------------------------------------------------------------
-- server-side rendering
----------------------------------------------------------------------

newtype HtmlPage a = HtmlPage a
    deriving (Show, Eq)

instance L.ToHtml a => L.ToHtml (HtmlPage a) where
    toHtmlRaw = L.toHtml
    toHtml (HtmlPage x) = L.doctypehtml_ $ do
        L.head_ $ do
            L.meta_ [L.charset_ "utf-8"]
            L.link_ [L.rel_ "stylesheet", L.href_ "styles.css"]
            L.meta_ [L.name_ "viewport"
                    ,L.content_ "width=device-width, initial-scale=1, shrink-to-fit=no"]
            L.with 
                (L.script_ mempty) 
                [L.src_ "all.js", L.async_ mempty, L.defer_ mempty] 
        L.body_ $ L.toHtml x

