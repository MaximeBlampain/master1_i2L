{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Baltig.Routes where

import Data.Proxy (Proxy(..))
import Miso
import Miso.String (ms, MisoString)
import Network.URI (URI)
import Servant.API
import Servant.Links

import Baltig.Action
-- import Baltig.News

uriToMs :: URI -> MisoString
uriToMs uri =
    let prefix = if uriAuthority uri == Nothing then "/" else ""
    in ms (prefix <> show uri)

----------------------------------------------------------------------
-- server api
----------------------------------------------------------------------



----------------------------------------------------------------------
-- client api
----------------------------------------------------------------------

type HomeRoute = View Action
type GitlabRoute = "gitlab" :> View Action

type ClientRoutes
    =    HomeRoute
    :<|> GitlabRoute

homeRoute, gitlabRoute :: URI
homeRoute = linkURI $ safeLink (Proxy @ClientRoutes) (Proxy @HomeRoute)
gitlabRoute = linkURI $ safeLink (Proxy @ClientRoutes) (Proxy @GitlabRoute)

