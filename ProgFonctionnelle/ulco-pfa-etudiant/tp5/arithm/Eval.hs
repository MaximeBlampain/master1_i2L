module Eval where

import Expr

eval :: Expr -> Int
eval (Val x) = x
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

