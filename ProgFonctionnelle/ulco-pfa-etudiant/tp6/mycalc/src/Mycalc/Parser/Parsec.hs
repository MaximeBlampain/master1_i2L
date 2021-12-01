module Mycalc.Parser.Parsec where

import Data.Char

import Mycalc.Syntax

-------------------------------------------------------------------------------
-- main types & functions
-------------------------------------------------------------------------------

type Result v = Either String (v, String)

newtype Parser v = Parser { runParser :: String -> Result v }

parseExpr :: String -> Either String Expr
parseExpr str = fst <$> runParser additiveP str

mapFst :: (a -> a') -> (a, b) -> (a', b)
mapFst f (x, y) = (f x, y)

itemP :: Parser Char 
itemP = Parser $ \s0 ->
    case s0 of
        (x:xs) -> Right (x, xs)
        [] -> Left "no input"

-------------------------------------------------------------------------------
-- parsing primitives
-------------------------------------------------------------------------------

digitP :: Parser Char
digitP = Parser $ \s0 ->
    case runParser itemP s0 of
        Right (c, cs) ->
            if isDigit c
                then Right (c, cs)
                else Left "not a digit"
        Left err -> Left err

digitsP :: Parser String
digitsP = Parser $ \s0 ->
    case runParser digitP s0 of
        Right (v1, s1) -> case runParser digitsP s1 of
            Right (v2, s2) -> Right (v1:v2, s2)
            Left _ -> Right ([v1], s1)
        Left err -> Left err

natP :: Parser Int
natP = Parser $ \s0 -> mapFst read <$> runParser digitsP s0

-------------------------------------------------------------------------------
-- grammar
-------------------------------------------------------------------------------

decimalP :: Parser Expr
decimalP = Parser $ \s0 -> mapFst ExprVal <$> runParser natP s0

multitiveP :: Parser Expr
multitiveP = Parser alt1 where

    -- Multitive <- Primary '*' Multitive
    alt1 s0 = case runParser decimalP s0 of
             Right (vleft, s1) -> case s1 of
                 ('*':s2) -> case runParser multitiveP s2 of
                     Right (vright, s3) -> Right (ExprMul vleft vright, s3)
                     _ -> alt2 s0
                 _ -> alt2 s0
             _ -> alt2 s0

    -- Multitive <- Primary
    alt2 s0 = runParser decimalP s0 

-- TODO
additiveP :: Parser Expr
additiveP = Parser $ \s ->
    if s == "0" then Right (ExprVal 0, "") else Left "waiting 0"

