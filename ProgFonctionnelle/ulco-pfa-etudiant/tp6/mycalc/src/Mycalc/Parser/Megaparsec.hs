module Mycalc.Parser.Megaparsec where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Mycalc.Syntax

-------------------------------------------------------------------------------
-- main types & functions
-------------------------------------------------------------------------------

type Parser = Parsec Void String

parseExpr :: String -> Either String Expr
parseExpr s0 = case parse additiveP "" s0 of
    Left bundle -> Left (errorBundlePretty bundle)
    Right v -> Right v

-------------------------------------------------------------------------------
-- parsing primitives
-------------------------------------------------------------------------------

natP :: Parser Int
natP = decimal

-------------------------------------------------------------------------------
-- grammar
-------------------------------------------------------------------------------

-- TODO decimalP :: Parser Expr

-- TODO multitiveP :: Parser Expr

-- TODO
additiveP :: Parser Expr
additiveP = char '0' >> (return $ ExprVal 0)

