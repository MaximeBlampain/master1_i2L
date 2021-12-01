{-# LANGUAGE OverloadedStrings #-}
<<<<<<< HEAD

=======
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Data.Aeson
>>>>>>> try to fix git error
import qualified Data.Text as T

data Person = Person
    { firstname    :: T.Text
    , lastname     :: T.Text
    , birthyear    :: T.Text
    , speakenglish :: Bool
<<<<<<< HEAD
    } deriving (Show)

main :: IO ()
main = do
    let res0 = Person "John" "Doe" "1970" False
    print res0

=======
    } deriving (Generic, Show)

instance FromJSON Person


main :: IO ()
main = do
    res1 <- eitherDecodeFileStrict "aeson-test1.json"
    print (res1:: Either String Person)

    res2 <- eitherDecodeFileStrict "aeson-test2.json"
    print (res2:: Either String Person)

    res3 <- eitherDecodeFileStrict "aeson-test3.json"
    print (res3:: Either String Person)



{-without generic 

{-# LANGUAGE OverloadedStrings #-}
{-
import Data.Aeson
import qualified Data.Text as T

data Person = Person
    { firstname    :: T.Text
    , lastname     :: T.Text
    , birthyear    :: T.Text
    , speakenglish :: Bool
    } deriving (Generic, Show)

instance FromJSON Person where
    parseJSON = withObject "Person" $ \v -> Person
    <$> v .: "firstname"
    <*> v .: "lastname"
    <*> v .: "birthyear"
    <*> v .: "speakenglish"


main :: IO ()
main = do
    res1 <- eitherDecodeFileStrict "aeson-test1.json"
    print (res1:: Either String Person)

    res2 <- eitherDecodeFileStrict "aeson-test2.json"
    print (res2:: Either String Person)

    res3 <- eitherDecodeFileStrict "aeson-test3.json"
    print (res3:: Either String Person)

-}
>>>>>>> try to fix git error
