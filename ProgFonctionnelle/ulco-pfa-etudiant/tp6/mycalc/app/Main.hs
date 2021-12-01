import Control.Monad
import System.Environment
import System.IO

import Mycalc.Codegen
import Mycalc.Syntax
import Mycalc.Parser.Megaparsec
import Mycalc.Parser.Monadic
import Mycalc.Parser.Parsec
import Mycalc.Parser.Recursive

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["megaparsec"] -> putStrLn "\nmegaparsec" >> run Mycalc.Parser.Megaparsec.parseExpr
        ["monadic"]    -> putStrLn "\nmonadic"    >> run Mycalc.Parser.Monadic.parseExpr
        ["parsec"]     -> putStrLn "\nparsec"     >> run Mycalc.Parser.Parsec.parseExpr
        ["recursive"]  -> putStrLn "\nrecursive"  >> run Mycalc.Parser.Recursive.parseExpr
        _ -> putStrLn "usage: <megaparsec|monadic|parsec|recursive>"

run :: (String -> Either String Expr) -> IO ()
run p = do
    putStr "\n> "
    hFlush stdout
    line <- getLine
    when (line /= "") $ do
        case p line of
            Left err -> putStrLn $ "error: " ++ err
            Right expr -> do
                print expr
                putStrLn $ toLisp expr
                print $ eval expr
        run p

