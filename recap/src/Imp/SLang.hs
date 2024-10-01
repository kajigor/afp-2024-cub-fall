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
    GEQ i li ->
    GEQ i ri ->
    L.Op ->
    SafeExpr Int li ->
    SafeExpr Int ri ->
    SafeExpr Int i
  Read :: SafeExpr Int i
  Write :: SafeExpr Int i -> SafeExpr () i
  Seq ::
    GEQ i li ->
    GEQ i ri ->
    SafeExpr () li ->
    SafeExpr t ri ->
    SafeExpr t i
  If ::
    GEQ i ci ->
    GEQ i ti ->
    GEQ i ei ->
    SafeExpr Int ci ->
    SafeExpr a ti ->
    SafeExpr a ei ->
    SafeExpr a i
  LetIn ::
    GEQ i ei ->
    SafeExpr Int ei ->
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
evalExpr igie (BinOp iegli iegri op le re) = do
  x <- evalExpr (geqTrans igie iegli) le
  y <- evalExpr (geqTrans igie iegri) re
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
evalExpr igie (Seq iegli iegri le re) =
  evalExpr (geqTrans igie iegli) le
    >> evalExpr (geqTrans igie iegri) re
evalExpr igie (If iegic iegit iegie c t e) = do
  x <- evalExpr (geqTrans igie iegic) c
  case x of
    0 -> evalExpr (geqTrans igie iegit) t
    _ -> evalExpr (geqTrans igie iegie) e
evalExpr igie (LetIn iegvie ve e) = do
  x <- evalExpr (geqTrans igie iegvie) ve
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
compile (TL.Const x) _ = return $ Const x
compile (TL.BinOp op e1 e2) i = do
  e1c <- compile e1 i
  e2c <- compile e2 i
  return $ BinOp geqRefl geqRefl op e1c e2c
compile TL.Read _ = return Read
compile (TL.Write e) i = do
  ec <- compile e i
  return $ Write ec
compile (TL.Seq e1 e2) i = do
  e1c <- compile e1 i
  e2c <- compile e2 i
  return $ Seq geqRefl geqRefl e1c e2c
compile (TL.If c t e) i = do
  cc <- compile c i
  tc <- compile t i
  ec <- compile e i
  return $ If geqRefl geqRefl geqRefl cc tc ec
compile (TL.LetIn s e1 e2) i = do
  e1c <- compile e1 i
  ns <- gets (Cons s)
  e2c <- lift $ evalStateT (compile e2 (S i)) ns
  return $ LetIn geqRefl e1c e2c
compile TL.Skip _ = return Skip
