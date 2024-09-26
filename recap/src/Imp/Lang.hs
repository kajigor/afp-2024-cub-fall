module Imp.Lang where

import Control.Monad.State
  ( MonadState (get),
    MonadTrans (lift),
    StateT,
    modify,
  )
import Control.Monad.Trans.Except (ExceptT, throwE)
import qualified Data.Map.Strict as M
import Text.Read (readMaybe)

-- An expression of the imperative language Imp
data Expr
  = Var String -- a variable; if it's not defined in the varMap -- throw an exception
  | Const Int -- a constant
  | BinOp Op Expr Expr -- a binary operator
  | Read -- reads from the standard input (getLine)
  | Write Expr -- writes to the standard output (putStrLn)
  | Seq Expr Expr -- sequence of two expressions
  | If Expr Expr Expr -- normal if expression; 0 means True; only computes one branch
  | LetIn String Expr Expr -- let-in expression; defines a new variable
  | Skip -- empty program; does nothing
  deriving (Eq, Show)

-- Binary operators
data Op
  = Plus -- (+)
  | Minus -- (-)
  deriving (Eq, Show)

-- Variable mapping; shadowing is allowed
type VarMap = M.Map String Int

-- Possible errors reported during evaluation
data Error = TypeError | UndefinedVar String | ParsingErr String
  deriving (Eq, Show)

-- Evaluation monad
type EvalM = StateT VarMap (ExceptT Error IO)

data ReturnValue = VInt Int | VUnit
  deriving (Eq, Show)

evalExpr :: Expr -> EvalM ReturnValue
evalExpr (Var s) = do
  varMap <- get
  case M.lookup s varMap of
    Just x -> return (VInt x)
    Nothing -> lift $ throwE $ UndefinedVar s
evalExpr (Const x) = return (VInt x)
evalExpr (BinOp op e1 e2) = do
  x <- evalExpr e1
  y <- evalExpr e2
  case (op, x, y) of
    (_, VUnit, _) -> lift $ throwE TypeError
    (_, _, VUnit) -> lift $ throwE TypeError
    (Plus, VInt x, VInt y) -> return $ VInt (x + y)
    (Minus, VInt x, VInt y) -> return $ VInt (x - y)
evalExpr Read = do
  input <- lift $ lift getLine
  case readMaybe input of
    Just x -> return (VInt x)
    Nothing -> lift $ throwE $ ParsingErr input
evalExpr (Write e) = do
  x <- evalExpr e
  case x of
    VUnit -> lift $ throwE TypeError
    VInt x -> lift $ lift $ print x
  return VUnit
evalExpr (Seq e1 e2) = evalExpr e1 >> evalExpr e2
evalExpr (If e c1 c2) = do
  x <- evalExpr e
  case x of
    VUnit -> lift $ throwE TypeError
    VInt 0 -> evalExpr c1
    VInt _ -> evalExpr c2
evalExpr (LetIn s e1 e2) = do
  x <- evalExpr e1
  case x of
    VUnit -> lift $ throwE TypeError
    VInt x -> modify $ M.insert s x
  evalExpr e2
evalExpr Skip = return VUnit
