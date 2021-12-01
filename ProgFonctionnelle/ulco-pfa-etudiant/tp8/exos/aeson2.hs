{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Aeson

data Person = Person
    { firstname    :: T.Text
    , lastname     :: T.Text
    , birthyear    :: Int
    } deriving (Generic, Show)

instance ToJSON Person
    toJSON (Person firstname lastname birthyear) =
    object ["first" .= firstname, "birth" .= birthyear, "last" .= lastname]

persons :: [Person]
persons =
    [ Person "John" "Doe" 1970
    , Person "Haskell" "Curry" 1900
    ]

main::IO()
main = do
    encodeFile "out-aeson1.json" persons