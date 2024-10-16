{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Imp.SLang where

import Control.Monad.Trans.Except (ExceptT, throwE)
import qualified Imp.Lang as L
import qualified Imp.TLang as TL
import Text.Read (readMaybe)
import Control.Monad.RWS
import Control.Monad.State

data Z = Z

data S a = S a

data GEQ a b where
  GEQZ :: GEQ a Z
  GEQS :: GEQ a b -> GEQ (S a) (S b)

geqTrans :: GEQ a b -> GEQ b c -> GEQ a c
geqTrans GEQZ GEQZ = GEQZ
geqTrans _ GEQZ = GEQZ
geqTrans (GEQS x) (GEQS y) = GEQS $ geqTrans x y

class Numm a where
  geqRefl :: GEQ a a

instance Numm Z where
  geqRefl = GEQZ

instance (Numm a) => Numm (S a) where
  geqRefl = GEQS geqRefl

data SafeExpr t i where
  Var ::
    GEQ i (S iv) ->
    iv ->
    SafeExpr Int i
  Const :: Int -> SafeExpr Int i
  BinOp :: L.Op -> SafeExpr Int i -> SafeExpr Int i -> SafeExpr Int i
  Read :: SafeExpr Int i
  Write :: SafeExpr Int i -> SafeExpr () i
  Seq :: SafeExpr () i -> SafeExpr t i -> SafeExpr t i
  If :: SafeExpr Int i -> SafeExpr t i -> SafeExpr t i -> SafeExpr t i
  LetIn :: SafeExpr Int i -> SafeExpr t (S i) -> SafeExpr t i
  Skip :: SafeExpr () i


data Error = ParsingErr String
  deriving (Eq, Show)

data SizedList v s where
  Nil :: SizedList v Z
  Cons :: v -> SizedList v s -> SizedList v (S s)

type EvalM i = StateT (SizedList Int i) (ExceptT Error IO)

lookupList :: GEQ l (S i) -> i -> SizedList v l -> v
lookupList (GEQS geq) iv (Cons x xs) = case geq of
  GEQZ -> x
  GEQS _ -> (\(S v) -> lookupList geq v xs) iv

evalExpr :: GEQ i ie -> SafeExpr t ie -> EvalM i t
evalExpr geq (Var g ind) = gets $ lookupList (geqTrans geq g) ind
evalExpr _ (Const i) = return i
evalExpr geq (BinOp op e1 e2) = do
  evalE1 <- evalExpr geq e1
  evalE2 <- evalExpr geq e2
  case op of
    L.Plus -> return $ evalE1 + evalE2
    L.Minus -> return $ evalE1 - evalE2
evalExpr _ Read = do
  input <- lift $ lift getLine
  case readMaybe input of
    Just x -> return x
    Nothing -> lift $ throwE $ ParsingErr input
evalExpr geq (Write e) = do
  x <- evalExpr geq e
  lift $ lift $ print x
evalExpr geq (Seq s1 s2) = do
  evalExpr geq s1
  evalExpr geq s2
evalExpr geq (If c t f) = do
  cond <- evalExpr geq c
  case cond of
    0 -> evalExpr geq t
    _ -> evalExpr geq f
evalExpr geq (LetIn vr va) = do
  x <- evalExpr geq vr
  list <- get
  lift $ evalStateT (evalExpr (GEQS geq) va) (Cons x list)
evalExpr _ Skip = return ()

data SafeCompileError = UndefinedVar String
  deriving (Eq, Show)

type CompileM i m = StateT (SizedList String i) (ExceptT SafeCompileError m)

lookupVar :: SizedList String i -> String -> Maybe (SafeExpr Int i)
lookupVar (Cons x xs) v =
  if x == v
  then Just $ Var (GEQS GEQZ) Z
  else case lookupVar xs v of
      Just (Var geq i) -> Just $ Var (GEQS geq) (S i)
      _ -> Nothing
lookupVar Nil _ = Nothing

compile :: (Monad m) => (Numm i) => TL.TExpr t -> i -> CompileM i m (SafeExpr t i)
compile (TL.Var v) _ = get >>= (\list -> maybe (lift $ throwE $ UndefinedVar v) return (lookupVar list v))
compile (TL.Const c) _ = return $ Const c
compile (TL.BinOp op e1 e2) i = BinOp op <$> compile e1 i <*> compile e2 i
compile TL.Read _ = return Read
compile (TL.Write e) i = Write <$> compile e i
compile (TL.Seq e1 e2) i = Seq <$> compile e1 i <*> compile e2 i
compile (TL.If c t f) i = If <$> compile c i <*> compile t i <*> compile f i
compile (TL.LetIn s e1 e2) i = LetIn <$> compile e1 i <*> (get >>= \list -> lift $ evalStateT (compile e2 (S i)) (Cons s list))
compile TL.Skip _ = return Skip
