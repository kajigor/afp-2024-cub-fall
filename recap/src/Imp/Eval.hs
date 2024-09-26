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
evalExpr (Var s) = do
  varMap <- get
  case M.lookup s varMap of
    Just x -> return x
    Nothing -> lift $ throwE $ UndefinedVar s
evalExpr (Const x) = return x
evalExpr (BinOp op e1 e2) = do
  x <- evalExpr e1
  y <- evalExpr e2
  case op of
    Plus -> return $ x + y
    Minus -> return $ x - y
    Mult -> return $ x * y
    Div -> case y of
      0 -> lift $ throwE DivByZero
      _ -> return $ x `div` y

evalCom :: Com -> EvalM ()
evalCom (Assign s e) = do
  x <- evalExpr e
  modify $ M.insert s x
evalCom (Read s) = do
  input <- lift $ lift getLine
  case readMaybe input of
    Just x -> modify $ M.insert s x
    Nothing -> lift $ throwE $ ParsingErr input
evalCom (Write e) = do
  x <- evalExpr e
  lift $ lift $ print x
evalCom (Seq c1 c2) = evalCom c1 >> evalCom c2
evalCom (If e c1 c2) = do
  x <- evalExpr e
  if x == 0 then evalCom c1 else evalCom c2
evalCom Skip = return ()

evalProg :: Prog -> EvalM Int
evalProg (Prog c e) = evalCom c >> evalExpr e 

eval :: Prog -> VarMap -> IO (Either Error Int)
eval program state = runExceptT (evalStateT (evalProg program) state)

runProgram :: Prog -> IO ()
runProgram program = do
  result <- eval program M.empty
  case result of
    Left err -> print err
    Right x -> putStrLn $ printf "Result: %s" (show x)
  putStrLn ""
