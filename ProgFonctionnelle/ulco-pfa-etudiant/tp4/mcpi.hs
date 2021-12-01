
import Data.List
import System.Random

data McpiState = McpiState
    { nwins :: Int
    , nsims :: Int
    , gen :: StdGen
    }

random01 :: StdGen -> (Double, StdGen)
random01 = random

-- iterMcpi :: McpiState -> (Double, McpiState)

-- run3 :: McpiState -> ([Double], McpiState)

-- runN :: Int -> McpiState -> ([Double], McpiState)

main :: IO ()
main = do
    state0 <- McpiState 0 0 <$> getStdGen
    putStrLn "TODO"

