module Parse (parse) where

import Expr

parse :: String -> (Expr, String)
parse str = 
    let (e, xs) = parseAux (words str)
    in (e, unwords xs)

parseAux :: [String] -> (Expr, [String])
parseAux [] = error "no parse"
parseAux (x:xs0) =
    let (e1, xs1) = parseAux xs0
        (e2, xs2) = parseAux xs1
    in case x of
        "+" -> (Add e1 e2, xs2)
        "*" -> (Mul e1 e2, xs2)
        _   -> (Val (read x), xs0)

