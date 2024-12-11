{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
module Lib where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except
import Data.List (isPrefixOf)
import Control.Monad.State (MonadTrans(..))
import GHC.List (uncons)
import Control.Applicative (Alternative(..))
import Control.Monad.Identity (Identity)
import Control.Monad (void)
import qualified Data.Map.Lazy as M

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- data Error = ParseException String deriving Monoid

-- instance Semigroup Error where
--   (<>) :: Error -> Error -> Error
--   (<>) _ y = y

type Parser a = StateT String [] a

runParser :: Parser a -> String -> [a]
runParser p s = fst <$> runStateT p s

chars :: String -> Parser String
chars cs = do
    s <- get
    if cs `isPrefixOf` s then return cs else empty

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
    s <- get
    case uncons s of
        Just (c, s') -> if f c then return c else empty
        Nothing -> empty

whitespace :: Parser ()
whitespace = void (many (chars " "))

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
  Const :: Float -> Expr Float
  UnOp :: UnaryOp -> Expr Float -> Expr Float
  BinOp :: BinaryOp -> Expr Float -> Expr Float -> Expr Float
  Seq :: Expr () -> Expr a -> Expr a
  Let :: String -> Expr Float -> Expr ()
  Skip :: Expr ()

data Error = UndefinedVar String | TypeError deriving (Eq, Show)

type VarMap = M.Map String Float
type TEvalM = StateT VarMap (ExceptT Error IO)

data ReturnValue = VFloat Float | VUnit deriving (Eq, Show)

evalExpr :: Expr a -> TEvalM ReturnValue
evalExpr (Var s) = do
  varMap <- get
  case M.lookup s varMap of
    Just x -> return (VFloat x)
    Nothing -> lift $ throwE $ UndefinedVar s
evalExpr (Const x) = return (VFloat x)
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
example = Seq (Let "a" (BinOp Add (Const 1.4) (UnOp Sin (Const 3.4)))) (BinOp Div (Var "a") (Const 3.5))

r :: Expr a -> IO (Either Error ReturnValue)
r expr = runExceptT $ evalStateT (evalExpr expr) M.empty