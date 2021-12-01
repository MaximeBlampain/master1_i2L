module Expr where

data Expr
    = Val Int
    | Add Expr Expr
    | Mul Expr Expr
    deriving (Eq, Show)

