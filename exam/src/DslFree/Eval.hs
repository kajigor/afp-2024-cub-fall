module DslFree.Eval where 

import DslFree.Lang 
import Control.Monad.Free 
import MyMonadTransformers.Lang (BinOp (..), UnOp (..))
import Control.Monad.State
    ( modify, put, get, evalStateT, MonadState(get), MonadTrans(lift), StateT, State, evalState, runState, execState)
-- import MyMonadTransformers.Eval
import qualified Data.Map.Strict as M
import Text.Printf (printf)

type VarMap = M.Map String Float
type Stack = [Float]
type ExecState = (Stack, VarMap)

evalBinOp :: BinOp -> Float -> Float -> Float
evalBinOp Plus = (+)
evalBinOp Minus = (-)
evalBinOp Mult = (*)
evalBinOp Div = (/)

evalUnOp :: UnOp -> Float -> Float
evalUnOp Sin = sin
evalUnOp Cos = cos
evalUnOp Asin = asin
evalUnOp Acos = acos
evalUnOp Tan = tan
evalUnOp Atan = atan

interpretStep :: StepDslFunctor a -> State (ExecState, Float) a
interpretStep (Put x next) = do
    ((stack, vars), r) <- get
    put ((x : stack, vars), x)
    return next
interpretStep (Store name next) = do
    ((stack, vars), _) <- get
    case stack of
        (x : xs) -> put ((stack, M.insert name x vars), x)
        []       -> error "Store: Stack is empty, cannot store value"
    return next
interpretStep (Recall name next) = do
    ((stack, vars), _) <- get
    case M.lookup name vars of
        Just value -> put ((value : stack, vars), value)
        Nothing    -> error $ "Recall: Variable " ++ name ++ " not found"
    return next
interpretStep (Un op next) = do
    ((stack, vars), _) <- get
    case stack of
        (x : xs) -> put ((evalUnOp op x : xs, vars), evalUnOp op x)
        []       -> error "Unary operation: Stack is empty"
    return next
interpretStep (Bin op next) = do
    ((stack, vars), _) <- get
    case stack of
        (y : x : xs) -> put ((evalBinOp op x y : xs, vars), evalBinOp op x y)
        _            -> error "Binary operation: Stack has fewer than two elements"
    return next

transform :: StepItem -> State (ExecState, Float) Float 
transform = foldFree interpretStep

calc :: StepItem -> (ExecState, Float) -> Float
calc step state = 
    case runState (transform step) state of
        (a, (b, c)) -> c 

calcState :: StepItem -> (ExecState, Float) -> ExecState
calcState step state = 
    case runState (transform step) state of
        (a, (b, c)) -> b

runCalc :: StepItem -> IO ()
runCalc step = do
  let result = calc step (([], M.empty), 0)
  printf "Result: %s" (show result)
--   putStrLn (show $ calcState step (([], M.empty), 0))
  putStrLn ""
