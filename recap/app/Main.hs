module Main (main) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (MonadTrans (lift), evalStateT)
import qualified Data.Map.Strict as M
import Imp.Exprs
import qualified Imp.Lang as L
import qualified Imp.SLang as SL
import qualified Imp.TLang as TL

assertEq a b = if a == b then return () else error ("Assertion failed: " ++ show a ++ " /= " ++ show b)

check expr result = do
  result' <- runExceptT $ evalStateT (L.evalExpr expr) M.empty
  assertEq result result'

checkT expr result = do
  result' <- runExceptT $ runExceptT evalT
  assertEq result result'
  where
    evalT = do
      compiledExpr <- TL.compile expr
      case compiledExpr of
        TL.IntExpr e -> do
          result <- lift $ evalStateT (TL.evalExpr e) M.empty
          return $ L.VInt result
        TL.UnitExpr e -> do
          lift $ evalStateT (TL.evalExpr e) M.empty
          return L.VUnit

checkS expr result = do
  result' <- runExceptT $ runExceptT $ runExceptT evalS
  assertEq result result'
  where
    evalS = do
      expr <- TL.compile expr
      case expr of
        TL.IntExpr expr -> do
          expr <- lift $ evalStateT (SL.compile expr SL.Z) SL.Nil
          result <- lift $ lift $ evalStateT (SL.evalExpr SL.GEQZ expr) SL.Nil
          return $ L.VInt result
        TL.UnitExpr expr -> do
          expr <- lift $ evalStateT (SL.compile expr SL.Z) SL.Nil
          lift $ lift $ evalStateT (SL.evalExpr SL.GEQZ expr) SL.Nil
          return L.VUnit

main :: IO ()
main = do
  check exprCorrect (Right $ L.VInt 3)
  check exprTypeError (Left L.TypeError)
  check exprUndefinedVar (Left $ L.UndefinedVar "y")
  checkT exprCorrect (Right $ Right $ L.VInt 3)
  checkT exprTypeError (Right $ Left TL.TypeError)
  checkT exprUndefinedVar (Left $ TL.UndefinedVar "y")
  checkS exprCorrect (Right $ Right $ Right $ L.VInt 3)
  checkS exprTypeError (Right $ Right $ Left TL.TypeError)
  checkS exprUndefinedVar (Right $ Left $ SL.UndefinedVar "y")
  checkS expr1 (Right $ Right $ Right $ L.VInt 9)
  checkS expr2 (Right $ Right $ Right $ L.VInt 4)
