{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
module Imp.Eval where

import Control.Monad.State
    ( modify, evalStateT, MonadState(get), MonadTrans(lift), StateT)
import Control.Monad.Trans.Except ( runExceptT, throwE, ExceptT)
import Control.Monad (when)
import qualified Data.Map.Strict as M
import Text.Printf (printf)
import Text.Read (readMaybe)
import Imp.Lang

-- Variable mapping; shadowing is allowed
type VarMap = M.Map String Int

-- Possible errors reported during evaluation 
data Error = UndefinedVar String | ParsingErr String | DivByZero deriving (Show)

-- Evaluation monad
type EvalM = StateT VarMap (ExceptT Error IO)

mapOp :: M.Map Op (Int -> Int -> Int)
mapOp = M.fromList [(Plus, (+)), (Minus, (-)), (Mult, (*)), (Div, div)]

evalExpr :: Expr -> EvalM Int
evalExpr (Var x) = do
  mapVar <- get
  when (M.notMember x mapVar) $ lift (throwE $ UndefinedVar x)
  return $ mapVar M.! x
evalExpr (Const x) = return x
evalExpr (BinOp op l r) = do
  x <- evalExpr l
  y <- evalExpr r
  when (y == 0 && op == Div) $ lift (throwE DivByZero)
  return $ (mapOp M.! op) x y

readSafe :: (Read a, MonadTrans t, Monad m, Monad (t (ExceptT Error m))) => String -> t (ExceptT Error m) a
readSafe str =
  case readMaybe str of
    Just n -> return n
    Nothing -> lift $ throwE $ ParsingErr str

evalCom :: Com -> EvalM ()
evalCom (Seq cm1 cm2) = mapM_ evalCom [cm1, cm2]
evalCom (Assign x e) = evalExpr e >>= \v -> modify $ M.insert x v
evalCom (Read x) = ((lift $ lift getLine) >>= readSafe) >>= (modify . M.insert x)
evalCom (Write e) = evalExpr e >>= \v -> lift $ lift $ print v
evalCom (If e cm1 cm2) = evalExpr e >>= \v -> evalCom (if v == 0 then cm1 else cm2)
evalCom Skip = return ()

evalProg :: Prog -> EvalM Int
evalProg (Prog com e) = evalCom com >> evalExpr e

eval :: Prog -> VarMap -> IO (Either Error Int)
eval program state = runExceptT (evalStateT (evalProg program) state)

runProgram :: Prog -> IO ()
runProgram program = do
  result <- eval program M.empty
  case result of
    Left err -> print err
    Right x -> putStrLn $ printf "Result: %s" (show x)
  putStrLn ""
