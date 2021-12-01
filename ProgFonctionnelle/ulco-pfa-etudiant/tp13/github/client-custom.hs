{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (eitherDecode, FromJSON)
import Network.Connection (TLSSettings (..))
import Network.HTTP.Client.Conduit (Manager)
import Network.HTTP.Conduit (httpLbs, mkManagerSettings, newManager,
                             parseRequest, responseBody)
import Network.HTTP.Simple (setRequestHeader)
import Network.HTTP.Types.Header (hUserAgent)

import User
import Repo

query :: FromJSON a => Manager -> String -> IO (Either String a)
query manager url = do
    request <- setRequestHeader hUserAgent ["MyClient"] <$> parseRequest url
    eitherDecode . responseBody <$> httpLbs request manager

main :: IO ()
main = do
    manager <- newManager $ mkManagerSettings (TLSSettingsSimple True False False) Nothing

    -- https://api.github.com/users/juliendehos
    eUser <- query manager "https://api.github.com/users/juliendehos"
    print (eUser :: Either String User)

    -- https://api.github.com/repositories/137743929
    eRepo <- query manager "https://api.github.com/repositories/137743929"
    print (eRepo :: Either String Repo)

