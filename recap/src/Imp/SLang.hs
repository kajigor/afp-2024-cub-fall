{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Imp.SLang where

import Control.Monad.State (StateT, gets, MonadTrans (lift), modify, evalStateT)
import Control.Monad.Trans.Except (ExceptT, throwE)
import qualified Imp.TLang as TL
import qualified Imp.Lang as L
import Text.Read (readMaybe)
import Control.Applicative (liftA3)

data Z = Z

data S a = S a

data GEQ a b where
  GEQZ :: GEQ a Z
  GEQS :: GEQ a b -> GEQ (S a) (S b)

geqTrans :: GEQ a b -> GEQ b c -> GEQ a c
geqTrans _ GEQZ = GEQZ
geqTrans (GEQS l) (GEQS r) = GEQS $ geqTrans l r

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
  Seq :: SafeExpr () i -> SafeExpr t i -> SafeExpr t i
  If :: SafeExpr Int i -> SafeExpr t i -> SafeExpr t i -> SafeExpr t i
  LetIn :: SafeExpr Int i -> SafeExpr t (S i) -> SafeExpr t i
  Skip :: SafeExpr () i

data Error = ParsingErr String
  deriving (Eq, Show)

data SizedList v s where
  Nil :: SizedList v Z
  Cons :: v -> SizedList v s -> SizedList v (S s)

atSL :: GEQ s (S i) -> SizedList v s -> v
atSL (GEQS GEQZ) (Cons v _) = v
atSL (GEQS geq@(GEQS _)) (Cons _ sl) = atSL geq sl

lookupSL :: Eq v => v -> SizedList v s -> Maybe (SafeExpr Int s)
lookupSL _ Nil = Nothing
lookupSL v (Cons v' sl)
  | v == v' = Just $ Var (GEQS GEQZ) Z
  | otherwise = (\(Var geq i) -> Var (GEQS geq) (S i)) <$> lookupSL v sl

type EvalM i = StateT (SizedList Int i) (ExceptT Error IO)

evalExpr :: GEQ i ie -> SafeExpr t ie -> EvalM i t
evalExpr p (Var p' _) = gets $ atSL (geqTrans p p')
evalExpr _ (Const v) = return v
evalExpr p (BinOp op l r) = do
  l' <- evalExpr p l
  r' <- evalExpr p r
  return $ case op of
    L.Plus -> l' + r'
    L.Minus -> l' + r'
evalExpr _ Read = do
  input <- lift $ lift getLine
  case readMaybe input of
    Just x -> return x
    Nothing -> lift $ throwE $ ParsingErr input
evalExpr p (Write e) = do
  v <- evalExpr p e
  lift $ lift $ print v
evalExpr p (Seq c1 c2) = evalExpr p c1 >> evalExpr p c2
evalExpr p (If cond t f) = do
  cond' <- evalExpr p cond
  evalExpr p $ if cond' == 0 then t else f
evalExpr p (LetIn v e) = do
  v' <- evalExpr p v
  l' <- gets $ Cons v'
  lift $ evalStateT (evalExpr (GEQS p) e) l'
evalExpr _ Skip = return ()

data SafeCompileError = UndefinedVar String
  deriving (Eq, Show)

type CompileM i m = StateT (SizedList String i) (ExceptT SafeCompileError m)

compile :: (Monad m, Numm i) => TL.TExpr t -> i -> CompileM i m (SafeExpr t i)
compile (TL.Var s) _ = gets (lookupSL s) >>= maybe (lift $ throwE $ UndefinedVar s) return
compile (TL.Const v) _ = return $ Const v
compile (TL.BinOp op l r) i = liftA2 (BinOp op) (compile l i) (compile r i)
compile TL.Read _ = return Read
compile (TL.Write e) i = Write <$> compile e i
compile (TL.Seq c1 c2) i = liftA2 Seq (compile c1 i) (compile c2 i)
compile (TL.If cond t f) i = liftA3 If (compile cond i) (compile t i) (compile f i)
compile (TL.LetIn s v e) i = do
  v' <- compile v i
  l' <- gets $ Cons s
  e' <- lift $ evalStateT (compile e (S i)) l'
  return $ LetIn v' e'
compile TL.Skip _ = return Skip
