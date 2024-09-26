{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Imp.SLang where

import Control.Monad.State
  ( MonadState (get),
    MonadTrans (lift),
    StateT,
    evalStateT,
    gets,
  )
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import qualified Imp.Lang as L
import qualified Imp.TLang as TL
import Text.Read (readMaybe)
data Z = Z

data S a = S a

data GEQ a b where
  GEQZ :: GEQ a Z
  GEQS :: GEQ a b -> GEQ (S a) (S b)

geqTrans :: GEQ a b -> GEQ b c -> GEQ a c
geqTrans GEQZ GEQZ = GEQZ
geqTrans (GEQS _) GEQZ = GEQZ
geqTrans (GEQS x) (GEQS y) = GEQS (geqTrans x y)

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
  Const ::
    Int ->
    SafeExpr Int i
  BinOp ::
    L.Op ->
    SafeExpr Int i ->
    SafeExpr Int i ->
    SafeExpr Int i
  Read ::
    SafeExpr Int i
  Write ::
    SafeExpr Int i ->
    SafeExpr () i
  Seq ::
    SafeExpr () i ->
    SafeExpr t i ->
    SafeExpr t i
  If ::
    SafeExpr Int i ->
    SafeExpr t i ->
    SafeExpr t i ->
    SafeExpr t i
  LetIn ::
    String ->
    SafeExpr Int i ->
    SafeExpr t (S i) ->
    SafeExpr t i
  Skip ::
    SafeExpr () i


data Error = ParsingErr String
  deriving (Eq, Show)

data SizedList v s where
  Nil :: SizedList v Z
  Cons :: v -> SizedList v s -> SizedList v (S s)

type EvalM i = StateT (SizedList Int i) (ExceptT Error IO)

lookupSized :: GEQ i (S iv) -> SizedList Int i -> Int
lookupSized (GEQS GEQZ) (Cons x _) = x
lookupSized (GEQS geq@(GEQS _)) (Cons _ xs) = lookupSized geq xs

evalExpr :: GEQ i ie -> SafeExpr t ie -> EvalM i t
evalExpr _ (Const x) = do
  return x
evalExpr g0 (Var g1 _) =
  gets (lookupSized (geqTrans g0 g1))
evalExpr geq (BinOp op e1 e2) = do
  x <- evalExpr geq e1
  y <- evalExpr geq e2
  case op of
    L.Plus -> return $ x + y
    L.Minus -> return $ x - y
evalExpr _ Read = do
  input <- lift $ lift getLine
  case readMaybe input of
    Just x -> return x
    Nothing -> lift $ throwE $ ParsingErr input
evalExpr geq (Write e) =
  evalExpr geq e >>= lift . lift . print
evalExpr geq (Seq e1 e2) =
  evalExpr geq e1 >> evalExpr geq e2
evalExpr geq (If cond e1 e2) = do
  x <- evalExpr geq cond
  if x == 0 then evalExpr geq e1 else evalExpr geq e2
evalExpr geq (LetIn _ e1 e2) = do
  x <- evalExpr geq e1
  l <- get
  lift $ evalStateT (evalExpr (GEQS geq) e2) (Cons x l)
evalExpr _ Skip = return ()


data SafeCompileError = UndefinedVar String
  deriving (Eq, Show)

type CompileM i m = StateT (SizedList String i) (ExceptT SafeCompileError m)

data Indexed i where
  Indexed :: GEQ i (S iv) -> iv -> Indexed i


lookupVar :: Monad m => SizedList String i -> String -> ExceptT SafeCompileError m (Indexed i)
lookupVar Nil s = throwE $ UndefinedVar s
lookupVar (Cons v rest) s
  | v == s    = do 
    return (Indexed (GEQS GEQZ) Z)
  | otherwise = do
    Indexed geq index <- lookupVar rest s
    return (Indexed (GEQS geq) (S index))


compile :: (Monad m) => (Numm i) => TL.TExpr t -> i -> CompileM i m (SafeExpr t i)
compile (TL.Var s) a = do 
  l <- get
  (Indexed g id) <- lift $ lookupVar l s 
  return $ Var g id
compile (TL.Const x) _ = return $ Const x 
compile (TL.BinOp op e1 e2) a = BinOp op <$> compile e1 a <*> compile e2 a
compile TL.Read _ = return Read
compile (TL.Write e) a = Write <$> compile e a 
compile (TL.Seq e1 e2) a = Seq <$> compile e1 a <*> compile e2 a
compile (TL.If e c1 c2) a = If <$> compile e a <*> compile c1 a <*> compile c2 a
compile (TL.LetIn name e1 e2) a = do 
  e1C <- compile e1 a
  l <- get
  e2C <- lift $ evalStateT (compile e2 (S a)) (Cons name l)
  return $ LetIn name e1C e2C
compile TL.Skip _ = return Skip 
