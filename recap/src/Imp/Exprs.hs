module Imp.Exprs where

import Imp.Lang

exprCorrect = If (Const 1) (LetIn "x" (Const 4) (Var "x")) (Const 3)

exprTypeError = If (Write (Const 3)) (LetIn "x" (Const 4) (Var "x")) (Const 3)

exprUndefinedVar = If (Const 0) (LetIn "x" (Const 4) (Var "y")) (Const 3)

expr1 = LetIn "x" (Const 4) (LetIn "y" (Const 5) (BinOp Plus (Var "x") (Var "y")))

expr2 = LetIn "x" (Const 4) (LetIn "y" (Const 5) (Var "x"))

expr3 = LetIn "x" (Const 4) (LetIn "x" (Const 5) (Var "x"))

expr4 = LetIn "x" (Const 4) (BinOp Plus (Var "x") (Var "y"))
