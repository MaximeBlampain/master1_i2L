{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import              GHC.Generics
import              Lucid
import              Data.Aeson (ToJSON)
import              Network.Wai.Middleware.Static (addBase, staticPolicy)
import qualified    Data.Text.Lazy as L
import              Web.Scotty

indexPage :: Html()
indexPage = do
   doctype_
   html_ $ do
        head_ $ meta_ [charset_ "utf8"]
        body_ $ do
            h1_ "hello-scotty"
            ul_ $ do
                li_ $ a_ [href_ "/route1"] "/route1"
                li_ $ a_ [href_ "/route1/route2"] "/route1/route2"
                li_ $ a_ [href_ "/html1"] "/html1"
                li_ $ a_ [href_ "/json1"] "/json1"
                li_ $ a_ [href_ "/json2"] "/json2"
                li_ $ a_ [href_ "add1/20/22"] "add1/20/22"
                li_ $ a_ [href_ "add2/?x=20&y=22"] "add2/?x=20&y=22"
                li_ $ a_ [href_ "add2/?x=20"] "add2/?x=20"
                li_ $ a_ [href_ "/index"] "index"
                img_ [src_ "/bob.png"]


main :: IO ()
main = scotty 3000 $ do
  middleware $ staticPolicy $ addBase "static"
  get "/" $ html $ renderText indexPage
  get "/route1" $ text "this is /route1"
  get "/route1/route2" $ text "/route1/route2"
  get "/html1" $ html "<h1>html1</h1>"
  get "/json1" $ json (42::Int)
  get "/json2" $ json (Person "toto" 42)
  get "/add1/:x/:y" $ do
    x <- read <$> param "x"
    y <- read <$> param "y"
    json (x+y ::Int)
  get "/add2" $ do
    x <- read <$> param "x" `rescue` (\_ -> return "0")
    y <- read <$> param "y" `rescue` (\_ -> return "0")
    json (x+y :: Int)
  get "/index" $ redirect "/"


data Person = Person
    { _name :: L.Text
    , _year :: Int
    } deriving (Generic, Show)

instance ToJSON Person
