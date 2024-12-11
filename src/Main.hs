module RPNStack where

import Control.Applicative (Alternative (empty))
import Control.Monad (guard, when)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.State
import Data.Bifunctor qualified
import System.IO (isEOF)
import Text.Read (readMaybe)

data BinOp = Plus | Minus | Mul | Div
data UnOp = Sin | Cos

class CmdSYM repr where
  pushCmd :: Float -> repr
  binOpCmd :: BinOp -> repr
  unOpCmd :: UnOp -> repr

class MemSYM repr where
  saveCmd :: repr
  getCmd :: repr

unOp op = do
  x <- pop
  push $ op x

binOp op = do
  x <- pop
  y <- pop
  push (op y x)

instance CmdSYM (EvalM ()) where
  binOpCmd Plus = binOp (+)
  binOpCmd Minus = binOp (-)
  binOpCmd Mul = binOp (*)
  binOpCmd Div = do
    x <- pop
    if x == 0
      then
        throwError "Div by zero"
      else do
        y <- pop
        push $ y / x
  unOpCmd Sin = unOp sin
  unOpCmd Cos = unOp cos
  pushCmd = push


instance MemSYM (EvalM ()) where
  saveCmd =
    singletonStack >> do
      x <- gets $ head . fst
      modify (const x <$>)
  getCmd = do
    m <- gets snd
    push m

instance CmdSYM String where
  binOpCmd Plus = "+"
  binOpCmd Minus = "-"
  binOpCmd Mul = "*"
  binOpCmd Div = "/"
  unOpCmd Sin = "sin"
  unOpCmd Cos = "cos"
  pushCmd = show

instance MemSYM String where
  saveCmd = "save"
  getCmd = "get"

type Stack = [Float]

type EvalM = StateT (Stack, Float) (Except String)

push :: Float -> EvalM ()
push x = modify $ Data.Bifunctor.first (x :)

pop :: EvalM Float
pop = do
  (stack, m) <- get
  if null stack
    then
      throwError "Empty stack"
    else do
      put (tail stack, m)
      return (head stack)

singletonStack :: EvalM ()
singletonStack = do
  l <- gets $ length . fst
  if l == 1 then return () else throwError "Stack is not empty"

readSafe str = maybe (throwError "Parsing error") return (readMaybe str)

parse :: (CmdSYM t, MemSYM t) => String -> Except String [t]
parse expr = mapM go (words expr)
  where
    go "+" = return $ binOpCmd Plus
    go "-" = return $ binOpCmd Minus
    go "*" = return $ binOpCmd Mul
    go "/" = return $ binOpCmd Div
    go "save" = return saveCmd
    go "get" = return getCmd
    go "sin" = return $ unOpCmd Sin
    go "cos" = return $ unOpCmd Cos
    go t = do
      v <- readSafe t
      return $ pushCmd v

exec :: [EvalM ()] -> Except String Float
exec cmds = evalStateT (sequence_ cmds >> singletonStack >> pop) ([], 0)

view :: [String] -> String
view = unwords

main :: IO ()
main = do
  line <- getLine
  case runExcept $ parse line of
    Right cmds -> do
      case runExcept $ exec cmds of
        Right res -> print res
        Left err -> putStrLn $ "Error: " ++ err
      -- putStrLn $ view cmds
    Left err -> putStrLn $ "Error: " ++ err

-- save - save top of stack to memory cell
-- get - put saved value to the stack top

-- Examples:
--   "1 save get + cos 1 -"
--   "1 sin"
--   "0 cos"
--   "1 2 +"
--   "1 2 *"
--   "1 2 -"
--   "1 2 + 3 * 4 -"

-- Errors:
--   "a 2 +"
--   "2 +"
--   ""
--   "- 1 2"