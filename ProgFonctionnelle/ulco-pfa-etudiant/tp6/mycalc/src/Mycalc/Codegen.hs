module Mycalc.Codegen where

import Mycalc.Syntax

eval :: Expr -> Int
eval (ExprVal v) = v
eval (ExprAdd e1 e2) = eval e1 + eval e2
eval (ExprMul e1 e2) = eval e1 * eval e2

toLisp :: Expr -> String
toLisp (ExprVal v) = show v
toLisp (ExprAdd e1 e2) = "(+ " ++ toLisp e1 ++ " " ++ toLisp e2 ++ ")"
toLisp (ExprMul e1 e2) = "(* " ++ toLisp e1 ++ " " ++ toLisp e2 ++ ")"

