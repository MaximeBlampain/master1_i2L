{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import qualified Data.Text as T
import qualified GHC.Generics
import qualified Data.Aeson

data Person = Person
    { firstname    :: T.Text
    , lastname     :: T.Text
    , birthyear    :: Int
    } deriving (Generic, Show)

instance ToJSON Person

persons :: [Person]
persons =
    [ Person "John" "Doe" 1970
    , Person "Haskell" "Curry" 1900
    ]

main::IO()
main = do
    encodeFile "out-aeson1.json" persons
