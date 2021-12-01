
import Control.Monad (forM_)
import System.Random

main :: IO ()
main = 
    forM_ [1..10::Int] $ \i -> do
        x <- randomRIO (1, 100::Int)
        putStrLn $ "iteration " ++ show i ++ ": " ++ show x


