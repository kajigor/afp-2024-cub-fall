module Imp.Eval where

import Control.Monad.State
    ( modify, gets, evalStateT, MonadTrans(lift), StateT )
import Control.Monad.Trans.Except ( runExceptT, throwE, ExceptT )
import qualified Data.Map.Strict as M
import Text.Printf (printf)
import Text.Read (readEither)
import Imp.Lang
import Control.Monad (when)

-- Variable mapping; shadowing is allowed
type VarMap = M.Map String Int

-- Possible errors reported during evaluation 
data Error = UndefinedVar String | ParsingErr String | DivByZero deriving (Show)

-- Evaluation monad
type EvalM = StateT VarMap (ExceptT Error IO)

evalExpr :: Expr -> EvalM Int
evalExpr (Var x) = do
  mv <- gets $ M.lookup x
  lift $ maybe (throwE (UndefinedVar x)) return mv
evalExpr (Const c) = return c
evalExpr (BinOp op l r) = do
  lv <- evalExpr l
  rv <- evalExpr r
  when (op == Div && rv == 0) (lift $ throwE DivByZero)
  return $ case op of
    Plus -> lv + rv
    Minus -> lv - rv
    Mult -> lv * rv
    Div -> lv `div` rv

evalCom :: Com -> EvalM ()
evalCom (Assign n e) = do
  v <- evalExpr e
  modify $ M.insert n v
evalCom (Read n) = do
  i <- lift $ lift getLine
  either (lift . throwE . ParsingErr) (modify . M.insert n) $ readEither i
evalCom (Write e) = do
  v <- evalExpr e
  lift $ lift $ print v
evalCom (Seq l r) = evalCom l >> evalCom r
evalCom (If e l r) = do
  v <- evalExpr e
  if v == 0 then evalCom l else evalCom r
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
