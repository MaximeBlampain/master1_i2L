import Control.Monad (when)

import Eval
import Parse

main :: IO ()
main = do
    putStrLn "\ninput ?"
    line <- getLine
    when (line /= "") $ do
        let (e, _xs) = parse line
        print e
        print $ eval e
        main

