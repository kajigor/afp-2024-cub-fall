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

throw = lift . throwE

evalExpr :: Expr -> EvalM Int
evalExpr (Var x) = do
  varMap <- get
  case M.lookup x varMap of
    Just x -> return x
    Nothing -> throw (UndefinedVar x)
evalExpr (Const n) = return n
evalExpr (BinOp op expr1 expr2) = do
  e1 <- evalExpr expr1
  e2 <- evalExpr expr2
  case op of
    Plus -> return (e1 + e2)
    Minus -> return (e1 - e2)
    Mult -> return (e1 * e2)
    Div -> case e2 of
      0 -> throw DivByZero
      _ -> return (e1 `div` e2)

evalCom :: Com -> EvalM ()
evalCom (Assign name expr) = do
  value <- evalExpr expr
  modify $ M.insert name value
evalCom (Read name) = do
  value <- lift $ lift getLine
  case readMaybe value of
    Just value -> evalCom $ Assign name $ Const value
    Nothing -> throw (ParsingErr value)
evalCom (Write expr) = do
  value <- evalExpr expr
  lift $ lift $ print value
evalCom (Seq com1 com2) = do
  evalCom com1
  evalCom com2
evalCom (If expr com1 com2) = do
  cond <- evalExpr expr
  if cond == 0 then
    evalCom com1
  else 
    evalCom com2
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
