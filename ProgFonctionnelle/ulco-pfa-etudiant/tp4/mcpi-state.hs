
import Control.Monad.State
import System.Random

data McpiState = McpiState
    { nwins :: Int
    , nsims :: Int
    , gen :: StdGen
    }

random01 :: StdGen -> (Double, StdGen)
random01 = random

-- iterMcpi :: State McpiState Double

-- run3 :: State McpiState [Double]

-- runN :: Int -> State McpiState [Double]

main :: IO ()
main = do
    state0 <- McpiState 0 0 <$> getStdGen
    -- TODO


