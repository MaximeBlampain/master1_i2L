{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import qualified Data.Text as T
import GHC.Generics
import Data.Yaml

data Person = Person
    { firstname    :: T.Text
    , lastname     :: T.Text
    , birthyear    :: Int
	, address	   :: Address
    } deriving (Generic, Show)
	
	

instance ToJSON Person

data Address = Address
	{
		number 		:: Int
		, road		:: T.Text
		, zipcode	:: Int
		, city		:: T.Text
	} deriving (Generic, Show)
	
instance ToJSON Address

persons :: [Person]
persons =
    [ Person "John" "Doe" 1970 
		(Address 42 "Pont Vieux" 43000 "ESpaly")
    , Person "Haskell" "Curry" 1900
		(Address 1337 "Pere Lachaise" 75000 "Paris")
    ]

main :: IO ()
main = do
	encodeFile "out-aeson3.yaml" persons
