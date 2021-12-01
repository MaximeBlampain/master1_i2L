{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.Aeson

data Person = Person
    { firstname    :: T.Text
    , lastname     :: T.Text
    , birthyear    :: Int
    } deriving (Show)

instance FromJSON Person where
    parseJSON = withObject "Person" $ \v -> Person
        <$> v .: "firstname"
        <*> v .: "lastname"
        <*> (read <$> v .: "birthyear")

main :: IO ()
main = do
    res1 <- eitherDecodeFileStrict "aeson-test1.json"
    print (res1:: Either String Person)

    res2 <- eitherDecodeFileStrict "aeson-test2.json"
    print (res2:: Either String Person)

    res3 <- eitherDecodeFileStrict "aeson-test3.json"
    print (res3:: Either String Person)


-- FOR YAML

{-# LANGUAGE OverloadedStrings #-}
{-}
import qualified Data.Text as T
import Data.Yaml

data Person = Person
    { firstname    :: T.Text
    , lastname     :: T.Text
    , birthyear    :: Int
    } deriving (Show)

instance FromJSON Person where
    parseJSON = withObject "Person" $ \v -> Person
        <$> v .: "firstname"
        <*> v .: "lastname"
        <*> (read <$> v .: "birthyear")

main :: IO ()
main = do
    res1 <- decodeFileEither "yaml-test1.yaml"
    print (res1:: Either ParseException Person)


-}
