module Mycalc.Syntax where

data Expr
    = ExprVal Int
    | ExprAdd Expr Expr
    | ExprMul Expr Expr
    deriving (Eq, Show)

