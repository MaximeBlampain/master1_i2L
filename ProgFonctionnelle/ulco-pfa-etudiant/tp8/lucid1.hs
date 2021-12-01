{-# LANGUAGE OverloadedStrings #-}
import Lucid

myHtml = (h1_ "hello" <> p_ "word") :: Html ()

main :: IO()
main = do
    print myHtml