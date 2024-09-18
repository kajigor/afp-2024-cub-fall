module Imp.Eval where

import Control.Monad.State
    ( modify, evalStateT, MonadState(get), MonadTrans(lift), StateT, put)
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
evalExpr (Var v) =  do
    varMap <- get 
    case  M.lookup v varMap of
        Just value -> return value 
        Nothing -> lift $ throwE $ UndefinedVar ("There is no " ++ v ++ " in map")
evalExpr (BinOp op expr1 expr2) = do
  res1 <- evalExpr expr1
  res2 <- evalExpr expr2
  if (op == Div) && (res2 == 0)
  then lift $ throwE DivByZero
  else return $ 
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
evalCom (Assign str expr) = do
  map <- get
  val <- evalExpr expr
  put (M.insert str val map)
evalCom (Read str) = do
  line <- lift $ lift getLine
  case readMaybe line of
    Just lineInt -> do
       map <- get
       put (M.insert str lineInt map)
    Nothing -> lift $ throwE $ ParsingErr ("Wrong input: " ++ line ++ ". Int was expected")

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
