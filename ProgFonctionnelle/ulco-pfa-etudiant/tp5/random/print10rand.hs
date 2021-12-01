import System.Random.MWC
import Data.Vector.Unboxed as V

main :: IO ()
main = do
  vs <- withSystemRandom . asGenST $ \gen -> uniformVector gen 10
  V.mapM_ print (vs :: Vector Int)

