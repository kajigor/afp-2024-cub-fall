{-# LANGUAGE GADTs #-}
module Interpreter where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except
import Control.Monad.State (MonadTrans(..))
import qualified Data.Map.Lazy as M
import Control.Monad.Identity (Identity (runIdentity))

data BinaryOp
  = Add
  | Sub
  | Mul
  | Div
  deriving (Eq, Show)

evalBinaryOp :: BinaryOp -> Float -> Float -> Float
evalBinaryOp Add x y = x + y
evalBinaryOp Sub x y = x - y
evalBinaryOp Mul x y = x * y
evalBinaryOp Div x y = x / y

data UnaryOp
  = Tan
  | Sin
  | Cos
  deriving (Eq, Show)

evalUnaryOp :: UnaryOp -> Float -> Float
evalUnaryOp Cos a = cos a
evalUnaryOp Sin a = sin a
evalUnaryOp Tan a = tan a

data Expr a where
  Var :: String -> Expr Float
  Val :: Float -> Expr Float
  UnOp :: UnaryOp -> Expr Float -> Expr Float
  BinOp :: BinaryOp -> Expr Float -> Expr Float -> Expr Float
  Seq :: Expr () -> Expr a -> Expr a
  Let :: String -> Expr Float -> Expr ()
  Skip :: Expr ()

data Error = UndefinedVar String | TypeError deriving (Eq, Show)

type VarMap = M.Map String Float
type EvalM = StateT VarMap (ExceptT Error Identity)

data ReturnValue = VFloat Float | VUnit deriving (Eq, Show)

evalExpr :: Expr a -> EvalM ReturnValue
evalExpr (Var s) = do
  varMap <- get
  case M.lookup s varMap of
    Just x -> return (VFloat x)
    Nothing -> lift $ throwE $ UndefinedVar s
evalExpr (Val x) = return (VFloat x)
evalExpr (UnOp op e1) = do
    x <- evalExpr e1
    case x of
        VFloat x' -> return $ VFloat $ evalUnaryOp op x'
        VUnit -> lift $ throwE TypeError
evalExpr (BinOp op e1 e2) = do
  x <- evalExpr e1
  y <- evalExpr e2
  case (x, y) of
    (VFloat x, VFloat y) -> return $ VFloat $ evalBinaryOp op x y
    _ -> lift $ throwE TypeError
evalExpr (Seq e1 e2) = evalExpr e1 >> evalExpr e2
evalExpr Skip = return VUnit
evalExpr (Let var e) = do
    ev <- evalExpr e
    case ev of
        VFloat x -> do
            modify $ M.insert var x
            return VUnit
        _ -> lift $ throwE TypeError

example :: Expr Float
example = Seq (Let "a" (BinOp Add (Val 1.4) (UnOp Sin (Val 3.4)))) (BinOp Div (Var "a") (Val 3.5))

example2 :: Expr Float
example2 = Seq (Let "a" (BinOp Add (Val 2.4) (UnOp Sin (Val 3.4)))) (BinOp Div (Var "a") (Val 3.5))

runExpr :: Expr a -> Either Error ReturnValue
runExpr expr = runIdentity $ runExceptT $ evalStateT (evalExpr expr) M.empty
