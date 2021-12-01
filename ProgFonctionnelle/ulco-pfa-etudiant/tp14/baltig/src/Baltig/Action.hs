module Baltig.Action where

-- import Miso.String
import Network.URI (URI)

-- import Baltig.News

data Action
    = None
    | SetUri URI
    | ChangeUri URI
    deriving (Eq)

