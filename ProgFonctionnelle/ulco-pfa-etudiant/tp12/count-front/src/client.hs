{-# LANGUAGE OverloadedStrings #-}

import Miso
import Miso.String

data Model = Model
    { _val :: Int
    } deriving (Eq)

data Action
    = ActionInc
    | ActionDec
    | ActionNone
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
updateModel ActionInc m = noEff m { _val = (_val m) + 1 }
updateModel ActionDec m = noEff m { _val = (_val m) - 1 }
updateModel ActionNone m = noEff m

viewModel :: Model -> View Action
viewModel m =
    div_ []
        [ h1_ [ ] [ text "count-front" ]
        , p_ [] [ text ("val = " <> ms (_val m)) ]
        , button_ [ onClick ActionInc ] [ text "+" ]
        , button_ [ onClick ActionDec ] [ text "-" ]
        ]

