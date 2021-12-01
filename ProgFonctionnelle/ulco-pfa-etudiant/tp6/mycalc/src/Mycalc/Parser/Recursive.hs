module Mycalc.Parser.Recursive where

import Data.Char

import Mycalc.Syntax

-------------------------------------------------------------------------------
-- main types & functions
-------------------------------------------------------------------------------

type Result v = Either String (v, String)

parseExpr :: String -> Either String Expr
parseExpr str = case pAdditive str of
    Right (v, _) -> Right v
    Left err -> Left err

-------------------------------------------------------------------------------
-- parsing primitives
-------------------------------------------------------------------------------

pDigit :: String -> Result Char
pDigit (c:cs) =
    if isDigit c
        then Right (c, cs)
        else Left "not a digit"
pDigit _ = Left "no input"

pDigits :: String -> Result String
pDigits s0 = case pDigit s0 of
    Right (v1, s1) -> case pDigits s1 of
        Right (v2, s2) -> Right (v1:v2, s2)
        Left _ -> Right ([v1], s1)
    Left err -> Left err

pNat :: String -> Result Int
pNat s0 = case pDigits s0 of
    Right (v, s1) -> Right (read v, s1)
    Left err -> Left err

-------------------------------------------------------------------------------
-- grammar
-------------------------------------------------------------------------------

pDecimal :: String -> Result Expr
pDecimal s0 = case pNat s0 of
    Right (v, s1) -> Right (ExprVal v, s1)
    Left err -> Left err

pMultitive :: String -> Result Expr
pMultitive s0 = alt1 where

    -- Multitive <- Primary '*' Multitive
    alt1 = case pDecimal s0 of
             Right (vleft, s1) -> case s1 of
                 ('*':s2) -> case pMultitive s2 of
                     Right (vright, s3) -> Right (ExprMul vleft vright, s3)
                     _ -> alt2
                 _ -> alt2
             _ -> alt2

    -- Multitive <- Primary
    alt2 = pDecimal s0 

-- TODO
pAdditive :: String -> Result Expr
pAdditive s0 = 
    if s0 == "0" then Right (ExprVal 0, "") else Left "waiting 0"

