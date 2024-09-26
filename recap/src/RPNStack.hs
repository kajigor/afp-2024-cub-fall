{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with maybe" #-}
module RPNStack where

import Control.Monad.State
import Control.Monad (when, guard)
import Text.Read (readMaybe)
import Control.Applicative (Alternative(empty))

-- Evaluator for formulas written in Reverse Polish Notation 
type Stack = [Int]

-- Use a monad stack to keep track of possible errors 
type EvalM = StateT Stack Maybe
--                          ^ inner (base) monad  

push :: Int -> EvalM ()
push x = modify (x:)

-- lift :: (MonadTrans t, Monad m) => m a -> t m a
--                                Maybe a -> (StateT Stack) Maybe a
pop :: EvalM Int
pop = do
  stack <- get
  when (null stack) $ lift Nothing -- guard (not $ null stack)
  put (tail stack)
  return (head stack)

singletonStack :: EvalM ()
singletonStack = do
  l <- gets length
  guard (l == 1)

readSafe str = 
  case readMaybe str of 
    Just n -> return n 
    Nothing -> empty 

eval :: String -> Maybe Int
eval expr =
    evalStateT go []
  where
    go = mapM_ step (words expr) >> singletonStack >> pop
    step "+" = binOp (+)
    step "-" = binOp (-)
    step "*" = binOp (*)
    step t = readSafe t >>= push
    binOp op = do
      x <- pop
      y <- pop
      push (op y x)

main :: IO ()
main = do
  print $ eval "1 2 +"         -- Just 3
  print $ eval "1 2 *"         -- Just 2
  print $ eval "1 2 -"         -- Just (-1)
  print $ eval "1 2 + 3 * 4 -" -- Just 5 

  print $ eval "a 2 +"         -- Nothing 
  print $ eval "2 +"           -- Nothing 
  print $ eval ""              -- Nothing 
  print $ eval "- 1 2"         -- Nothing 
