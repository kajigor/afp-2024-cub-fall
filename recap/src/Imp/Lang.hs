module Imp.Lang where

-- A program in the imperative language Imp 
data Prog
  = Prog Com Expr      -- Each program executes some IO action and results in a single number
  deriving (Eq, Show)

-- A command of the imperative language Imp 
data Com
  = Assign String Expr -- assignes the result of the expression to the variable
  | Read String -- reads from standard input (getLine); expects the input to be Int
  | Write Expr -- writes to the stardard output (putStrLn)
  | Seq Com Com -- sequence of two commands
  | If Expr Com Com -- normal if expression; 0 means True; only computes one branch
  | Skip -- empty program; does nothing
  deriving (Eq, Show)

-- An expression of the imperative language Imp 
data Expr
  = Var String -- a variable; if it's not defined in the varMap -- throw an exception
  | Const Int -- a constant
  | BinOp Op Expr Expr -- a binary operator
  deriving (Eq, Show)

-- Four possible binary operators 
data Op
  = Plus               -- (+)
  | Minus              -- (-)
  | Mult               -- (*)
  | Div                -- div 
  deriving (Eq, Show, Ord)
