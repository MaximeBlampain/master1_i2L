{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (eitherDecodeStrict, FromJSON)
import JavaScript.Web.XMLHttpRequest
import Miso
import Miso.String

import Baltig.Action as Action
import Baltig.Model
-- import Baltig.Routes
import Baltig.View
-- import Gitlab.Api

main :: IO ()
main = miso $ \myUri -> App
    { initialAction = Action.None
    , model         = createModel myUri
    , update        = updateModel
    , view          = viewModel
    , events        = defaultEvents
    , subs          = [ uriSub Action.SetUri ]
    , mountPoint    = Nothing
    , logLevel      = Off
    }

----------------------------------------------------------------------
-- update
----------------------------------------------------------------------

updateModel :: Action -> Model -> Effect Action Model

updateModel Action.None m = noEff m

updateModel (Action.SetUri myUri) m = noEff m { uri = myUri } 
updateModel (Action.ChangeUri myUri) m = m <# do
    pushURI myUri
    pure Action.None

----------------------------------------------------------------------
-- xhr
----------------------------------------------------------------------

_xhrUrl :: FromJSON a => MisoString -> IO [a]
_xhrUrl url = do
    let req = Request GET url Nothing [] False NoData
    mBstr <- contents <$> xhrByteString req
    case mBstr of
        Nothing -> do
            consoleLog "xhrUrl failed"
            return []
        Just bstr -> case eitherDecodeStrict bstr of
            Left err -> do
                consoleLog ("eitherDecodeStrict: " <> ms err)
                consoleLog ("bstr: " <> ms bstr)
                return []
            Right res -> return res

