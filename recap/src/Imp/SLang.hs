{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Imp.SLang where

import Control.Monad.State (MonadTrans (lift), StateT (runStateT), get, gets)
import Control.Monad.Trans.Except (ExceptT, throwE)
import qualified Imp.Lang as L
import qualified Imp.TLang as TL
import Text.Read (readMaybe)

data Z = Z

data S a = S a

data GEQ a b where
  GEQZ :: GEQ a Z
  GEQS :: GEQ a b -> GEQ (S a) (S b)

geqTrans :: GEQ a b -> GEQ b c -> GEQ a c
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
  Seq :: SafeExpr () i -> SafeExpr a i -> SafeExpr a i
  If :: SafeExpr Int i -> SafeExpr a i -> SafeExpr a i -> SafeExpr a i
  LetIn :: SafeExpr Int i -> SafeExpr a (S i) -> SafeExpr a i
  Skip :: SafeExpr () i

data Error = ParsingErr String
  deriving (Eq, Show)

data SizedList v s where
  Nil :: SizedList v Z
  Cons :: v -> SizedList v s -> SizedList v (S s)

getEl :: GEQ l (S i) -> i -> SizedList v l -> v
getEl (GEQS p) iv (Cons x xs) =
  case p of
    GEQZ -> x
    GEQS _ -> let (S y) = iv in getEl p y xs

getVar :: (Eq v) => v -> SizedList v l -> Maybe (SafeExpr Int l)
getVar _ Nil = Nothing
getVar a (Cons x xs) =
  if a == x
    then Just $ Var (GEQS GEQZ) Z
    else
      ( \case
          Var p b -> Var (GEQS p) (S b)
          _ -> undefined
      )
        <$> getVar a xs

type EvalM i = StateT (SizedList Int i) (ExceptT Error IO)

evalExpr :: GEQ i ie -> SafeExpr t ie -> EvalM i t
evalExpr g (Var g1 iv) = gets $ getEl (geqTrans g g1) iv
evalExpr _ (Const a) = return a
evalExpr g (BinOp op e1 e2) = do
  x <- evalExpr g e1
  y <- evalExpr g e2
  case op of
    L.Plus -> return $ x + y
    L.Minus -> return $ x - y
evalExpr _ Read = do
  input <- lift $ lift getLine
  case readMaybe input of
    Just x -> return x
    Nothing -> lift $ throwE $ ParsingErr input
evalExpr g (Write e) = do
  x <- evalExpr g e
  lift $ lift $ print x
evalExpr g (Seq c1 c2) = evalExpr g c1 >> evalExpr g c2
evalExpr g (If e c1 c2) = do
  x <- evalExpr g e
  if x == 0 then evalExpr g c1 else evalExpr g c2
evalExpr g (LetIn e1 e2) = do
  x <- evalExpr g e1
  l <- get
  lift $ fst <$> runStateT (evalExpr (GEQS g) e2) (Cons x l)
evalExpr _ Skip = return ()

data SafeCompileError = UndefinedVar String
  deriving (Eq, Show)

type CompileM i m = StateT (SizedList String i) (ExceptT SafeCompileError m)

compile :: (Monad m) => (Numm i) => TL.TExpr t -> i -> CompileM i m (SafeExpr t i)
compile (TL.Var s) _ = do
  x <- gets $ getVar s
  case x of
    Nothing -> lift $ throwE $ UndefinedVar s
    Just y -> return y
compile (TL.Const x) _ = return $ Const x
compile (TL.BinOp op e1 e2) i = BinOp op <$> compile e1 i <*> compile e2 i
compile TL.Read _ = return Read
compile (TL.Write e) i = Write <$> compile e i
compile (TL.Seq e1 e2) i = Seq <$> compile e1 i <*> compile e2 i
compile (TL.If e c1 c2) i = If <$> compile e i <*> compile c1 i <*> compile c2 i
compile (TL.LetIn s e1 e2) i = do
  var <- compile e1 i
  vars <- get
  LetIn var <$> lift (fst <$> runStateT (compile e2 (S i)) (Cons s vars))
compile TL.Skip _ = return Skip
