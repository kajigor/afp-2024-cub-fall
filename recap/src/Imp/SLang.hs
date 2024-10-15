{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Imp.SLang where

import Control.Monad.State (StateT (runStateT), MonadTrans(lift), get, put, evalStateT) 
import Control.Monad.Trans.Except (ExceptT, throwE)
import qualified Imp.Lang as L
import qualified Imp.TLang as TL
import Text.Read (readMaybe)
import Imp.Exprs (expr1, expr2)
import Data.IntMap (update)

data Z = Z

data S a = S a

data GEQ a b where
  GEQZ :: GEQ a Z
  GEQS :: GEQ a b -> GEQ (S a) (S b)

geqTrans :: GEQ a b -> GEQ b c -> GEQ a c
geqTrans _ GEQZ  = GEQZ
geqTrans (GEQS a) (GEQS b) = GEQS (geqTrans a b)

class Numm a where
  geqRefl :: GEQ a a

instance Numm Z where
  geqRefl = GEQZ

instance (Numm a) => Numm (S a) where
  geqRefl = GEQS geqRefl 

data SafeExpr t i where
  Var :: GEQ i (S iv) -> iv -> SafeExpr Int i
  Const :: Int -> SafeExpr Int i
  BinOp :: L.Op -> SafeExpr Int i -> SafeExpr Int i -> SafeExpr Int i
  Read :: SafeExpr Int i
  Write :: SafeExpr Int i -> SafeExpr () i
  Seq :: SafeExpr () i -> SafeExpr a i -> SafeExpr a i
  If :: SafeExpr Int i -> SafeExpr a i -> SafeExpr a i -> SafeExpr a i 
  LetIn :: SafeExpr Int i -> SafeExpr t (S i) -> SafeExpr t i
  Skip :: SafeExpr () i

data Error = ParsingErr String
  deriving (Eq, Show)

data SizedList v s where
  Nil :: SizedList v Z
  Cons :: v -> SizedList v s -> SizedList v (S s)

type EvalM i = StateT (SizedList Int i) (ExceptT Error IO)

getElFromList :: GEQ i (S iv) -> SizedList Int i  -> Int
-- (a + 1) (1 + Z) ==  GEQ i (S iv) => iv == Z
getElFromList (GEQS GEQZ) (Cons v list) = v
getElFromList (GEQS b@(GEQS _)) (Cons v list) = getElFromList b list

evalExpr :: GEQ i ie -> SafeExpr t ie -> EvalM i t
evalExpr g (Var g1 ind) = 
  do
  l <- get
  return (getElFromList (geqTrans g g1) l)
evalExpr g (LetIn expr1 expr2) = do
  l <- get
  val1 <- evalExpr g expr1
  lift $ evalStateT (evalExpr (GEQS g) expr2) (Cons val1 l)
evalExpr g Read = do
  line <- lift $ lift getLine
  case readMaybe line of
    Just lineInt -> return lineInt
    Nothing ->lift $ throwE (ParsingErr line)
evalExpr g (Const v) = return v
evalExpr g Skip = return ()
evalExpr g (If exp a b) = do
  val <- evalExpr g exp
  if val == 0
  then evalExpr g a
  else evalExpr g b
evalExpr g (BinOp op expr1 expr2) = do
  val1 <- evalExpr g expr1
  val2 <- evalExpr g expr2
  case op of 
    L.Plus -> return (val1 + val2)
    L.Minus -> return (val1 - val2)
evalExpr g (Write expr) = do
  val <- evalExpr g expr
  lift $ lift $ print val
evalExpr g (Seq expr1 expr2) = do 
  evalExpr g expr1
  evalExpr g expr2


data SafeCompileError = UndefinedVar String
  deriving (Eq, Show)

type CompileM i m = StateT (SizedList String i) (ExceptT SafeCompileError m)

getNumOfEl :: SizedList String i -> String -> Maybe (SafeExpr Int i)
getNumOfEl (Cons head tail) var = 
  if head == var
  then Just (Var (GEQS GEQZ) Z) 
  else fmap (\(Var geq ix) -> Var (GEQS geq) (S ix)) (getNumOfEl tail var)
getNumOfEl nil var = Nothing

compile :: (Monad m) => (Numm i) => TL.TExpr t -> i -> CompileM i m (SafeExpr t i)
compile (TL.Var s) i = do
  l <- get
  case getNumOfEl l s of
    Just res -> return res 
    Nothing -> lift $ throwE $ UndefinedVar s
compile (TL.Const var) i = return (Const var) 
compile (TL.BinOp op expr1 expr2) i = do
  comExpr1 <- compile expr1 i
  comExpr2 <- compile expr2 i
  return $ BinOp op comExpr1 comExpr2
compile TL.Read i = return Read 
compile (TL.Write expr) i = do
  compExpr <- compile expr i  
  return (Write compExpr)
compile (TL.Seq expr1 expr2) i = do
  comExpr1 <- compile expr1 i
  comExpr2 <- compile expr2 i
  return (Seq comExpr1 comExpr2)
compile (TL.If expr a b) i = do
  comExpr <- compile expr i
  comA <- compile a i
  comB <- compile b i
  return (If comExpr comA comB)
compile (TL.LetIn str expr1 expr2) i = do
  l <- get
  comExpr1 <- compile expr1 i
  let a = compile expr2 (S i)
  b <- lift $ evalStateT a (Cons str l)
  return $ LetIn comExpr1 b
compile TL.Skip i = return Skip
