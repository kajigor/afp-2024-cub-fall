module Imp.Eval where

import Control.Monad.State
    ( modify, evalStateT, MonadState(get), MonadTrans(lift), StateT )
import Control.Monad.Trans.Except ( runExceptT, throwE, ExceptT (..) )
import qualified Data.Map.Strict as M
import Text.Printf (printf)
import Text.Read (readMaybe, lift)
import Imp.Lang
import Control.Monad.Trans.Class (MonadTrans, lift)
-- import Control.Monad.Trans.IO ( IOT, fromIO )

-- Variable mapping; shadowing is allowed
type VarMap = M.Map String Int

-- Possible errors reported during evaluation 
data Error = UndefinedVar String | ParsingErr String | DivByZero deriving (Show)

-- Evaluation monad
type EvalM = StateT VarMap (ExceptT Error IO)

evalExpr :: Expr -> EvalM Int
evalExpr expr = do
  vars <- get

  case expr of
    Var varName -> case M.lookup varName vars of
      Just value -> Control.Monad.State.lift $ Control.Monad.Trans.Class.lift $ return value
      Nothing -> Control.Monad.State.lift $ throwE (UndefinedVar varName)

    Const value -> Control.Monad.State.lift $ Control.Monad.Trans.Class.lift $ return value

    BinOp op expr1 expr2 -> do
      result1 <- evalExpr expr1
      result2 <- evalExpr expr2

      Control.Monad.State.lift (case op of
        Plus -> Control.Monad.Trans.Class.lift $ return $ result1 + result2
        Minus -> Control.Monad.Trans.Class.lift $ return $ result1 - result2
        Mult -> Control.Monad.Trans.Class.lift $ return $ result1 * result2
        Div -> case result2 of 
          0 -> throwE DivByZero
          _ -> Control.Monad.Trans.Class.lift $ return $ div result1 result2
          )


evalCom :: Com -> EvalM ()
evalCom (Assign varName expr) = do
  result <- evalExpr expr

  modify $ M.insertWith const varName result


evalCom (Read value) = do
  valueString <- Control.Monad.State.lift $ Control.Monad.Trans.Class.lift getLine

  case readMaybe valueString :: Maybe Int of
    Just value -> modify $ M.insertWith const valueString value
    Nothing -> return ()


evalCom (Write expr) = do
  result <- evalExpr expr

  Control.Monad.State.lift $ Control.Monad.Trans.Class.lift $ printf $ show result

  return ()


evalCom (Seq com1 com2) = do
  result1 <- evalCom com1
  result2 <- evalCom com2

  return ()


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
