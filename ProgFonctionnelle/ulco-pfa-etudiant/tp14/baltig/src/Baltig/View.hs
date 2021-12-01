{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Baltig.View where

-- import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Miso
-- import Miso.String (ms, MisoString)
import Servant.API

import Baltig.Action
import Baltig.Model
-- import Baltig.News
import Baltig.Routes

----------------------------------------------------------------------
-- main definitions
----------------------------------------------------------------------

clientViews
    :: (Model -> View Action)
    :<|> (Model -> View Action)
clientViews
    =    homeView
    :<|> gitlabView

viewModel :: Model -> View Action
viewModel m = 
    case runRoute (Proxy @ClientRoutes) clientViews Baltig.Model.uri m of
        Left _ -> text "not found"
        Right v -> v

domHeader :: View Action
domHeader = 
    div_ [] 
        [ h1_ [] [text "baltig"]
        , button_ [onClick (Baltig.Action.ChangeUri homeRoute)] [text "home"]
        , "  "
        , button_ [onClick (Baltig.Action.ChangeUri gitlabRoute)] [text "gitlab"]
        , hr_ []
        ]

----------------------------------------------------------------------
-- home
----------------------------------------------------------------------

homeView :: Model -> View Action
homeView _ = 
    div_ []
        [ domHeader
        , h2_ [] [text "TODO home"]
        ]

----------------------------------------------------------------------
-- gitlab
----------------------------------------------------------------------

gitlabView :: Model -> View Action
gitlabView _ = 
    div_ [] 
        [ domHeader
        , h2_ [] [text "TODO gitlab"]
        ]

