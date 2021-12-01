{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Common where

import Data.Proxy (Proxy(..))
import Miso
import Miso.String (ms, MisoString)
import Network.URI (URI)
import Servant.API
import Servant.Links

----------------------------------------------------------------------
-- model 
----------------------------------------------------------------------

data Model = Model
    { _msg :: MisoString
    } deriving (Eq)

initModel :: Model
initModel = Model "initial model"

----------------------------------------------------------------------
-- action
----------------------------------------------------------------------

data Action
    = ActionNone
    deriving (Eq)

----------------------------------------------------------------------
-- view 
----------------------------------------------------------------------

homeView :: Model -> View Action
homeView m =
    div_ []
        [ h1_ [] [text "isohello"]
        , p_ [] [text (_msg m)]
        , p_ [] [a_ [href_ (uriToMs linkHello)] [text "hello"]]
        ]

----------------------------------------------------------------------
-- client routes 
----------------------------------------------------------------------

type ClientRoutes = HomeRoute

type HomeRoute = View Action

homeRoute :: URI
homeRoute = linkURI $ safeLink (Proxy @ClientRoutes) (Proxy @HomeRoute)

----------------------------------------------------------------------
-- api
----------------------------------------------------------------------

type HelloApi = "hello" :>  Get '[JSON] MisoString

----------------------------------------------------------------------
-- client api
----------------------------------------------------------------------

type ClientApi
    =    HelloApi  

linkHello :: URI
linkHello = linkURI $ safeLink (Proxy @ClientApi) (Proxy @HelloApi)

uriToMs :: URI -> MisoString
uriToMs = ms . show

