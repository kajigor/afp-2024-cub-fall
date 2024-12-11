module MyMonadTransformers.Eval where 

import Control.Monad.State
    ( modify, put, get, evalStateT, MonadState(get), MonadTrans(lift), StateT)
import Control.Monad.Trans.Except ( runExceptT, throwE, ExceptT)
import Control.Monad (when)
import qualified Data.Map.Strict as M
import Text.Printf (printf)
import Text.Read (readMaybe)
import Data.Bifunctor (second, first)
import MyMonadTransformers.Lang

type VarMap = M.Map String Float
type Stack = [Float]

data Error = UndefinedVar String | ParsingErr String | DivByZero | EmptyStack deriving (Show)

type EvalM = StateT (VarMap, Stack) (ExceptT Error IO)

push :: Float -> EvalM Float
push x = modify (second (x:)) >> return x 

pop :: EvalM Float
pop = do
  (vars, stack) <- get
  when (null stack) $ lift $ throwE EmptyStack
  put (vars, tail stack)
  return (head stack)

store :: String -> Float -> EvalM Float
store v x = modify (first (M.insert v x)) >> return x 

lookfor :: String -> EvalM Float
lookfor v = do
    (vars, _) <- get 
    case M.lookup v vars of 
        Just x -> push x
        _ -> lift $ throwE $ UndefinedVar v


evalUn :: UnOp -> Float -> EvalM Float 
evalUn Sin x = return $ sin x
evalUn Cos x = return $ cos x
evalUn Asin x = return $ asin x
evalUn Acos x = return $ acos x
evalUn Tan x = return $ tan x
evalUn Atan x = return $ atan x

evalBin :: BinOp -> Float -> Float -> EvalM Float 
evalBin Plus x y = return $ x + y 
evalBin Minus x y = return $ x - y 
evalBin Mult x y = return $ x * y 
evalBin Div x y = do 
    when (y == 0) $ lift (throwE DivByZero) 
    return $ x / y 


evalStep :: Step -> EvalM Float
evalStep (Bin op) = pop >>= \x -> pop >>= \y -> evalBin op y x >>= \res -> push res
evalStep (Un op) = pop >>= evalUn op >>= \res -> push res
evalStep (Store v) = pop >>= \x -> store v x >> push x 
evalStep (Recall v) = lookfor v
evalStep (Seq cm1 cm2) = evalStep cm1 >> evalStep cm2
evalStep (Put x) = push x


eval :: Step -> (VarMap, Stack) -> IO (Either Error Float)
eval program state = runExceptT (evalStateT (evalStep program) state)

runProgram :: Step -> IO ()
runProgram program = do
  result <- eval program (M.empty, [])
  case result of
    Left err -> print err
    Right x -> putStrLn $ printf "Result: %s" (show x)
  putStrLn ""
