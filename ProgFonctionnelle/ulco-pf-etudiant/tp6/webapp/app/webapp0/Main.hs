{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.IO.Class
import           Data.IORef
import qualified Data.Text.Lazy as L
import           Web.Scotty 

main :: IO ()
main = do

    scotty 5000 $ do

        get "/" $ do
            text "TODO"

        get "/add/:name" $ do
            name <- param "name"
            text $ name <> " added"

