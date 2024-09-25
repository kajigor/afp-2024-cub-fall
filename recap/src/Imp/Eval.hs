module Imp.Eval where

import Control.Monad.State
  ( MonadState (get),
    MonadTrans (lift),
    StateT,
    evalStateT,
    modify,
  )
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import qualified Data.Map.Strict as M
import Imp.Lang
import Text.Printf (printf)
import Text.Read (readMaybe)

-- Variable mapping; shadowing is allowed
type VarMap = M.Map String Int

-- Possible errors reported during evaluation
data Error = UndefinedVar String | ParsingErr String | DivByZero deriving (Show)

-- Evaluation monad
type EvalM = StateT VarMap (ExceptT Error IO)

evalExpr :: Expr -> EvalM Int
evalExpr (Var v) = do
  m <- get
  case m M.!? v of
    Nothing -> lift $ throwE $ UndefinedVar v
    Just x -> return x
evalExpr (Const x) = return x
evalExpr (BinOp op l r) = do
  x <- evalExpr l
  y <- evalExpr r
  x `opF` y
  where
    opF = case op of
      Plus -> (return .) . (+)
      Minus -> (return .) . (-)
      Mult -> (return .) . (*)
      Div -> \x y ->
        if y /= 0
          then
            return $ x `div` y
          else
            lift $ throwE DivByZero

evalCom :: Com -> EvalM ()
evalCom (Assign v e) = do
  x <- evalExpr e
  modify $ M.insert v x
evalCom (Read v) = do
  line <- lift $ lift getLine
  case readMaybe line of
    Nothing -> lift $ throwE $ ParsingErr $ "Can't parse number from: " ++ line
    Just x -> modify $ M.insert v x
evalCom (Write e) = do
  r <- evalExpr e
  lift . lift $ print r
evalCom (Seq c1 c2) = evalCom c1 >> evalCom c2
evalCom (If p t f) = do
  x <- evalExpr p
  if x == 0
    then
      evalCom t
    else
      evalCom f
evalCom Skip = return ()

evalProg :: Prog -> EvalM Int
evalProg (Prog com expr) = evalCom com >> evalExpr expr

eval :: Prog -> VarMap -> IO (Either Error Int)
eval program state = runExceptT (evalStateT (evalProg program) state)

runProgram :: Prog -> IO ()
runProgram program = do
  result <- eval program M.empty
  case result of
    Left err -> print err
    Right x -> putStrLn $ printf "Result: %s" (show x)
  putStrLn ""
