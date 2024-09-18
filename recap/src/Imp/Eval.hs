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
evalExpr (Const a) = return a
evalExpr (Var v) = undefined
evalExpr (BinOp op expr1 expr2) = do
  res1 <- evalExpr expr1
  res2 <- evalExpr expr2
  return $ 
    case op of
      Plus -> res1 + res2
      Minus -> res1 - res2
      Div -> res1 `div` res2
      Mult -> res1 * res2

evalCom :: Com -> EvalM ()
evalCom (Write expr) = do
  val <- evalExpr expr
  lift (lift $ print val)
evalCom (Seq com1 com2) = do
    evalCom com1 
    evalCom com2
evalCom (If expr com1 com2 ) = do
  val <- evalExpr expr
  if val == 0
  then evalCom com1 
  else evalCom com2 
evalCom Skip = return ()
evalCom (Assign str expr) = undefined
evalCom (Read str) = undefined

evalProg :: Prog -> EvalM Int
evalProg (Prog com expr) =  do
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
