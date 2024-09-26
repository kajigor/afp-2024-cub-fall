module Imp.Eval where

import Control.Monad (when, guard)
import Control.Monad.State
    ( modify, evalStateT, MonadState(get), MonadTrans(lift), StateT, liftIO )
import Control.Monad.Trans.Except ( runExceptT, throwE, ExceptT)
import Control.Monad.Except (throwError)
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

getMap :: String -> EvalM Int
getMap varName = do
    varMap <- get
    maybe (throwError (UndefinedVar varName)) return (M.lookup varName varMap)

setMap :: String -> Int -> EvalM ()
setMap varName value = modify (M.insert varName value)

readInt :: EvalM Int
readInt = do
    input <- liftIO getLine
    case readMaybe input of
        Just n  -> return n
        Nothing -> throwError (ParsingErr "expected Int")

evalExpr :: Expr -> EvalM Int
evalExpr expr = do
  case expr of
    Var str -> do -- a variable; if it's not defined in the varMap -- throw an exception
      val <- getMap str
      return (val)
    Const val -> do -- a constant
      return (val)
    BinOp op expr1 expr2 -> do -- a binary operators
      x <- evalExpr expr1
      y <- evalExpr expr2
      case op of
        Plus -> return (x + y)
        Minus -> return (x - y)
        Mult -> return (x * y)
        Div -> if y == 0
          then throwError DivByZero
          else return (x `div` y)

evalCom :: Com -> EvalM ()
evalCom com = do
  case com of
    Assign str expr -> evalExpr expr >>= setMap str -- assignes the result of the expression to the variable
    Read str -> readInt >>= setMap str -- reads from standard input (getLine); expects the input to be Int
    Write expr -> do -- writes to the stardard output (putStrLn)
      output <- evalExpr expr
      liftIO $ print output
    Seq com1 com2 -> do -- sequence of two commands
      evalCom com1
      evalCom com2
    If expr com1 com2 -> do -- normal if expression; 0 means True; only computes one branch
      exprResult <- evalExpr expr
      if exprResult == 0
        then evalCom com1
        else evalCom com2
    Skip -> return () -- empty program; does nothing

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