module Imp.Eval where

import Control.Monad.State
    ( modify, evalStateT, MonadState(get), MonadTrans, StateT )
import Control.Monad.Trans.Except ( runExceptT, throwE, ExceptT (..) )
import qualified Data.Map.Strict as M
import Text.Printf (printf)
import Text.Read (readMaybe)
import Imp.Lang
import Control.Monad.Trans.Class (MonadTrans, lift)

-- Variable mapping; shadowing is allowed
type VarMap = M.Map String Int

-- Possible errors reported during evaluation 
data Error = UndefinedVar String | ParsingErr String | DivByZero deriving (Show)

-- Evaluation monad
type EvalM = StateT VarMap (ExceptT Error IO)


evalBinaryExpr :: Integral a => Op -> a -> a -> a
evalBinaryExpr op val1 val2
  | op == Plus = val1 + val2
  | op == Minus = val1 - val2
  | op == Mult = val1 * val2
  | otherwise = div val1 val2 


evalExpr :: Expr -> EvalM Int
evalExpr (Var varName) = do
  vars <- get

  case M.lookup varName vars of
    Just value -> lift $ lift $ return value
    Nothing -> lift $ throwE (UndefinedVar varName)


evalExpr (Const value) = do
  lift $ lift $ return value


evalExpr (BinOp op expr1 expr2) = do
  result1 <- evalExpr expr1
  result2 <- evalExpr expr2

  lift (case op of
    Div | result2 == 0 -> throwE DivByZero
    _ -> lift $ return $ evalBinaryExpr op result1 result2
    )


evalCom :: Com -> EvalM ()
evalCom (Assign varName expr) = do
  result <- evalExpr expr

  modify $ M.insert varName result


evalCom (Read varName) = do
  valueString <- lift $ lift getLine

  case readMaybe valueString :: Maybe Int of
    Just value -> modify $ M.insert varName value
    Nothing -> return ()


evalCom (Write expr) = do
  result <- evalExpr expr

  lift $ lift $ print result


evalCom (Seq com1 com2) = do
  evalCom com1
  evalCom com2


evalCom (If expr com1 com2) = do
  result <- evalExpr expr

  case result of
    0 -> evalCom com1
    _ -> evalCom com2


evalCom Skip = do
  return ()


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
