{-# LANGUAGE OverloadedStrings #-}

module View where

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Lucid
--import TextShow (showtl)

import Model
import Databases

bootstrap :: T.Text
bootstrap = "https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css"

mkPage :: L.Text -> Html () -> L.Text
mkPage myTitle myHtml = renderText $ do
    doctype_
    html_ $ do
        head_ $ do
            meta_ [charset_ "utf-8"]
            meta_ [ name_ "viewport"
                  , content_ "width=device-width,initial-scale=1,shrink-to-fit=no"]
            link_ [ rel_ "stylesheet", href_ bootstrap]
            title_ $ toHtml myTitle
        body_ $ div_ [class_ "container"] myHtml

indexPage :: L.Text
indexPage = mkPage title $ do
    h1_ toHtml title
    p_ $ a_[href_ "messages"]"go to message"
    where title = "ulcoforum - index"

messagePage :: [Message] -> L.Text
messagePage ms = mkPage title $ do
    h1_ toHtml title
    ul_ $ mapM_ formatMessage ms
    p_ $ a_[href_ "/"]"go to index"
    where title = "ulcoforum - messages"
          formatMessage :: Message -> Html ()
          formatMessage m = li $ do
            toHtml $ _user m
            " : "
            toHtml $ _text m
