{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

import Network.Wai.Handler.Warp (run)
import Servant 

import MathApi

type ServerApi
    =    Math42
-- TODO

handleServerApi :: Server ServerApi
handleServerApi
    =    (return 42)
-- TODO

-- handleMathMul2 :: Int -> Handler Int
-- TODO

serverApp :: Application
serverApp = serve (Proxy @ServerApi) handleServerApi

main :: IO ()
main = do
    putStrLn "listening..."
    run 3000 serverApp

