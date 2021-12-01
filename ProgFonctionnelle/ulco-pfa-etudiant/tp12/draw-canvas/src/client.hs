{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Set as S
import qualified JavaScript.Web.Canvas as JSC
import Data.Map (singleton)
import Miso
import Miso.String (ms, MisoString)

----------------------------------------------------------------------
-- model 
----------------------------------------------------------------------

paramWidth, paramHeight :: Double
paramWidth = 400
paramHeight = 300

data Model = Model
    { _val :: Int
    } deriving (Eq)

----------------------------------------------------------------------
-- view 
----------------------------------------------------------------------

viewModel :: Model -> View Action
viewModel _m =
    div_ []
        [ h1_ [] [ text "draw-canvas" ]
        , p_ [] [ text "Use up/down/left/right to move... " ]
        , canvas_ [ id_ "mycanvas"
                  , width_ (ms paramWidth)
                  , height_ (ms paramHeight)
                  , style_  (singleton "border" "1px solid black")
                  ] []
        ]

draw :: Model -> IO ()
draw _m = do
    ctx <- jsGetCtx
    JSC.clearRect 0 0 paramWidth paramHeight ctx
    JSC.fillStyle 100 255 100 255 ctx
    JSC.fillRect 0 0 paramWidth paramHeight ctx
    JSC.stroke ctx

----------------------------------------------------------------------
-- action
----------------------------------------------------------------------

data Action
    = ActionNone
    | ActionDraw
    | ActionKey (S.Set Int)
    deriving (Eq)

updateModel :: Action -> Model -> Effect Action Model
updateModel ActionDraw m = m <# (draw m >> pure ActionNone)
updateModel ActionNone m = noEff m
updateModel (ActionKey ks) m = m <# (jsConsoleLog (ms $ show ks) >> pure ActionDraw)

----------------------------------------------------------------------
-- main
----------------------------------------------------------------------

main :: IO ()
main = startApp App
    { initialAction = ActionDraw
    , update        = updateModel
    , view          = viewModel
    , model         = Model 0 
    , subs          = [ keyboardSub ActionKey ]
    , events        = defaultEvents
    , mountPoint    = Nothing
    , logLevel      = Off
    }

----------------------------------------------------------------------
-- JS FFI
----------------------------------------------------------------------

foreign import javascript unsafe "$r = mycanvas.getContext('2d');"
    jsGetCtx :: IO JSC.Context

foreign import javascript unsafe "console.log($1);"
    jsConsoleLog :: MisoString -> IO ()

