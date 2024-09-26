module Imp.Programs where

import Imp.Lang

programs = [prog0, prog1, prog2, prog3, prog4, prog5, prog6, prog7, prog8, prog9]

-- 13
-- Result: 42
prog0 :: Prog
prog0 =
  Prog
    (Write (Const 13))
    (Const 42)

-- Result 55
prog1 :: Prog
prog1 =
  Prog
    ( Assign "x" (Const 13)
        `Seq` Assign "y" (BinOp Plus (Var "x") (Const 42))
        `Seq` If (Var "y") (Assign "x" (BinOp Plus (Var "x") (Const 1))) (Assign "y" (Var "y"))
    )
    (Var "y")

-- given, your input is a correct int number, the output is:
-- <your input>
-- Result: <your input>
prog2 :: Prog
prog2 =
  Prog
    (Read "x" `Seq` Write (Var "x"))
    (Var "x")

-- Error undefined var
prog3 :: Prog
prog3 =
  Prog (Write (Var "x")) (Var "x")

-- Error division by zero
prog4 :: Prog
prog4 =
  Prog Skip (BinOp Div (Const 13) (BinOp Minus (Const 13) (Const 13)))

-- 13
-- Result: 42
prog5 :: Prog
prog5 =
  Prog (If (Const 0) (Write (Const 13)) (Write (Var "x"))) (Const 42)

-- Error undefined var
prog6 :: Prog
prog6 =
  Prog (If (Const 1) (Write (Const 13)) (Write (Var "x"))) (Const 42)

-- program calculating the factorial of the 5 (without cycle)
-- Result: 120
prog7 :: Prog
prog7 =
  Prog
    ( Assign "x" (Const 5)
        `Seq` Assign "y" (Const 1)
        `Seq` Assign "y" (BinOp Mult (Var "y") (Var "x"))
        `Seq` Assign "x" (BinOp Minus (Var "x") (Const 1))
        `Seq` Assign "y" (BinOp Mult (Var "y") (Var "x"))
        `Seq` Assign "x" (BinOp Minus (Var "x") (Const 1))
        `Seq` Assign "y" (BinOp Mult (Var "y") (Var "x"))
        `Seq` Assign "x" (BinOp Minus (Var "x") (Const 1))
        `Seq` Assign "y" (BinOp Mult (Var "y") (Var "x"))
        `Seq` Assign "x" (BinOp Minus (Var "x") (Const 1))
        `Seq` Assign "y" (BinOp Mult (Var "y") (Var "x"))
    )
    (Var "y")

-- program calculating the modulo 7 of the input number
-- Result: <input> % 7
prog8 :: Prog
prog8 =
  Prog
    (Read "x")
    (BinOp Minus (Var "x") (BinOp Mult (BinOp Div (Var "x") (Const 7)) (Const 7)))

prog9 :: Prog
prog9 =
  Prog
    ( Read "x"
        `Seq` ( If
                  (BinOp Div (Var "x") (Const 2))
                  (Assign "y" (Const 1))
                  (Write (Const 42))
                  `Seq` Write (Var "y")
              )
    )
    (BinOp Plus (Var "x") (Var "y"))
