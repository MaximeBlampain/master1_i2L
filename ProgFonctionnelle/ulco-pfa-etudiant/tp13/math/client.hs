{-# LANGUAGE TypeApplications #-}

import Data.Proxy (Proxy(..))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client 

import MathApi

query42 :: ClientM Int
query42 = client (Proxy @Math42)

-- queryAdd :: Int -> Int -> ClientM Int
-- TODO

-- queryMul2 :: Int -> ClientM Int
-- TODO

-- query42mul2 :: ClientM (Int, Int)
-- query42mul2 = do
-- TODO

main :: IO ()
main = do
    myManager <- newManager defaultManagerSettings
    let myClient = mkClientEnv myManager (BaseUrl Http "localhost" 3000 "")

    runClientM query42 myClient >>= print

    -- TODO

