{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Imp.SLang where

import Control.Monad.State (StateT, evalStateT, gets, lift)
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
geqTrans (GEQS ab) (GEQS cd) = GEQS (geqTrans ab cd)

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
  BinOp ::
    L.Op ->
    SafeExpr Int i ->
    SafeExpr Int i ->
    SafeExpr Int i
  Read :: SafeExpr Int i
  Write :: SafeExpr Int i -> SafeExpr () i
  Seq ::
    SafeExpr () i ->
    SafeExpr t i ->
    SafeExpr t i
  If ::
    SafeExpr Int i ->
    SafeExpr a i ->
    SafeExpr a i ->
    SafeExpr a i
  LetIn ::
    SafeExpr Int i ->
    SafeExpr a (S i) ->
    SafeExpr a i
  Skip :: SafeExpr () i

data Error = ParsingErr String
  deriving (Eq, Show)

data SizedList v s where
  Nil :: SizedList v Z
  Cons :: v -> SizedList v s -> SizedList v (S s)

getElem :: GEQ s (S i) -> SizedList v s -> v
getElem (GEQS GEQZ) (Cons x _) = x
getElem (GEQS geq@(GEQS _)) (Cons _ xs) = getElem geq xs

data BoundBy i where
  BoundBy :: GEQ i (S iv) -> iv -> BoundBy i

findIdx :: (Eq v) => v -> SizedList v s -> Maybe (BoundBy s)
findIdx _ Nil = Nothing
findIdx v (Cons x xs)
  | x == v = Just $ BoundBy (GEQS GEQZ) Z
  | otherwise = fmap (\(BoundBy geq iv) -> BoundBy (GEQS geq) (S iv)) (findIdx v xs)

type EvalM i = StateT (SizedList Int i) (ExceptT Error IO)

evalExpr :: GEQ i ie -> SafeExpr t ie -> EvalM i t
evalExpr igie (Var igiv _) = do
  gets (getElem (geqTrans igie igiv))
evalExpr _ (Const x) = return x
evalExpr igie (BinOp op le re) = do
  x <- evalExpr igie le
  y <- evalExpr igie re
  case op of
    L.Plus -> return (x + y)
    L.Minus -> return (x - y)
evalExpr _ Read = do
  input <- lift $ lift getLine
  case readMaybe input of
    Just x -> return x
    Nothing -> lift $ throwE $ ParsingErr input
evalExpr igie (Write e) = do
  x <- evalExpr igie e
  lift $ lift $ print x
evalExpr igie (Seq le re) =
  evalExpr igie le >> evalExpr igie re
evalExpr igie (If c t e) = do
  x <- evalExpr igie c
  case x of
    0 -> evalExpr igie t
    _ -> evalExpr igie e
evalExpr igie (LetIn ve e) = do
  x <- evalExpr igie ve
  ns <- gets (Cons x)
  lift $ evalStateT (evalExpr (GEQS igie) e) ns
evalExpr _ Skip = return ()

data SafeCompileError = UndefinedVar String
  deriving (Eq, Show)

type CompileM i m = StateT (SizedList String i) (ExceptT SafeCompileError m)

compile :: (Monad m) => (Numm i) => TL.TExpr t -> i -> CompileM i m (SafeExpr t i)
compile (TL.Var s) _ = do
  vi <- gets (findIdx s)
  case vi of
    Just (BoundBy igiv iv) -> return $ Var igiv iv
    Nothing -> lift $ throwE $ UndefinedVar s
compile (TL.Const x) _ =
  return $ Const x
compile (TL.BinOp op e1 e2) i = do
  BinOp op <$> compile e1 i <*> compile e2 i
compile TL.Read _ =
  return Read
compile (TL.Write e) i =
  Write <$> compile e i
compile (TL.Seq e1 e2) i =
  Seq <$> compile e1 i <*> compile e2 i
compile (TL.If c t e) i =
  If <$> compile c i <*> compile t i <*> compile e i
compile (TL.LetIn s e1 e2) i = do
  e1c <- compile e1 i
  ns <- gets (Cons s)
  e2c <- lift $ evalStateT (compile e2 (S i)) ns
  return $ LetIn e1c e2c
compile TL.Skip _ = return Skip
