module Common where

import Control.Monad.State
import Text.Printf (printf)

data BinOp op = BinOp String op

instance Show (BinOp op) where
  show (BinOp n _) = n

formatBinOp :: BinOp op -> String -> String -> String
formatBinOp (BinOp op _) x y = printf "(%s %s %s)" x op y

getOp :: BinOp op -> op
getOp (BinOp _ f) = f

newtype VarState = VarState {getVar :: Int}

newVar :: State VarState Int
newVar = do
  n <- gets getVar
  put $ VarState {getVar = n + 1}
  return n

newtype S a = S {unS :: State VarState String}

newtype Env a = Env {unEnv :: a}