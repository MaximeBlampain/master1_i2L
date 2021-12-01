import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Numeric

inc :: Float
inc = 0.1

myShowFloat :: Float -> String
myShowFloat v = showFFloat (Just 1) v ""

type Model = Float

main :: IO ()
main = do
    let window = InWindow "hello gloss" (450, 450) (0, 0)
        model = 0
        fps = 30
    play window azure fps model hDraw hEvent hTime

hEvent :: Event -> Model -> Model
hEvent _ m = m + inc

hTime :: Float -> Model -> Model
hTime _ m = m

hDraw :: Model -> Picture
hDraw m = Scale 0.2 0.2 $ Text (myShowFloat m)

