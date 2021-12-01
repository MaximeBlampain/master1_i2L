import Control.Monad (forM_)
import Data.Array
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

type Ix2 = (Int, Int)
type Board = Array Ix2 Char
type Tictactoe = (Char, Char, Char, Board)

-- newBoard :: Board

-- newTictactoe :: Tictactoe

-- printTictactoe :: Tictactoe -> IO ()

-- playTictactoe :: Ix2 -> Tictactoe -> Tictactoe

-- findWinner :: Board -> Char

-- loopTictactoe :: Tictactoe -> IO Tictactoe

main :: IO ()
main = putStrLn "TODO"

