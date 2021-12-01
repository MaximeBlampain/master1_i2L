{-# LANGUAGE OverloadedStrings #-}

import Miso

import Common

updateModel :: Action -> Model -> Effect Action Model
updateModel ActionNone m = noEff m

main :: IO ()
main = miso $ \_uri -> App
    { initialAction = ActionNone
    , update        = updateModel
    , view          = homeView
    , model         = initModel
    , subs          = []
    , events        = defaultEvents
    , mountPoint    = Nothing
    , logLevel = Off
    }

