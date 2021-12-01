{-# LANGUAGE OverloadedStrings #-}

module Baltig.Model where

-- import Miso.String
import Network.URI (URI)

-- import Baltig.News

data Model = Model
    { uri           :: URI
    } deriving (Eq)

createModel :: URI -> Model
createModel myUri = Model myUri

