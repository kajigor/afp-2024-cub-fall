module Imp.Eval where

import Control.Monad.State
    ( modify, evalStateT, MonadState(get), MonadTrans(lift), StateT )
import Control.Monad.Trans.Except ( runExceptT, throwE, ExceptT )
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

evalExpr :: Expr -> EvalM Int
evalExpr (Var variable) = do
  myMap <- get
  let value = M.lookup variable myMap
  case value of
    Just value -> return value
    Nothing -> lift $ throwE (UndefinedVar variable)

evalExpr (Const int) = return int

evalExpr (BinOp op lhs rhs) = do
  lhsResult <- evalExpr lhs
  rhsResult <- evalExpr rhs
  case op of
    Plus -> return $ lhsResult + rhsResult
    Minus -> return $ lhsResult - rhsResult
    Mult -> return $ lhsResult * rhsResult
    Div -> case rhsResult of
      0 -> lift $ throwE DivByZero
      _ -> return $ div lhsResult rhsResult


evalCom :: Com -> EvalM ()
evalCom (Assign var expr) = do
  result <- evalExpr expr
  modify $ M.insert var result

evalCom (Read var) = do
  value <- lift $ lift getLine
  case readMaybe value of
    Just val -> modify $ M.insert var val
    Nothing -> lift $ throwE (ParsingErr value)

evalCom (Write expr) = do
  result <- evalExpr expr
  lift $ lift $ print result

evalCom (Seq comL comR) = do
  evalCom comL
  evalCom comR

evalCom (If expr comTrue comFalse) = do
  result <- evalExpr expr
  case result of
    0 -> evalCom comTrue
    _ -> evalCom comFalse

evalCom Skip = return ()

evalProg :: Prog -> EvalM Int
evalProg (Prog com expr) = do
  evalCom com
  evalExpr expr

eval :: Prog -> VarMap -> IO (Either Error Int)
eval program state = runExceptT (evalStateT (evalProg program) state)

runProgram :: Prog -> IO ()
runProgram program = do
  result <- eval program M.empty
  case result of
    Left err -> print err
    Right x -> putStrLn $ printf "Result: %s" (show x)
  putStrLn ""
