{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Gitlab.Api where 

-- import Data.Proxy (Proxy(..))
-- import Data.Text
import Network.URI (URI, URIAuth(..))
import Servant.API
-- import Servant.Links

toRemote :: URI -> URI
toRemote uri = uri
    { uriScheme = "https:"
    , uriAuthority = Just (URIAuth "" "gitlab.com/" "")
    }

