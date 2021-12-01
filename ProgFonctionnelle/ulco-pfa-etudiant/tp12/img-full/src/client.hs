{-# LANGUAGE OverloadedStrings #-}

import Miso
import Miso.String (ms)

data Model = Model
    { _val :: Int
    } deriving (Eq)

viewModel :: Model -> View Action
viewModel m =
    div_ []
        [ h1_ [] [ text "img-full" ]
        , p_ [] [ text ("_val = " <> ms (_val m)) ]
        ]

data Action
    = ActionNone
    deriving (Eq)

main :: IO ()
main = startApp App
    { initialAction = ActionNone
    , update        = updateModel
    , view          = viewModel
    , model         = Model 0
    , subs          = []
    , events        = defaultEvents
    , mountPoint    = Nothing
    , logLevel      = Off
    }

updateModel :: Action -> Model -> Effect Action Model
updateModel ActionNone m = noEff m

