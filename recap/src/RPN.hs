module RPN where

import Control.Monad.State

-- Evaluator for formulas written in Reverse Polish Notation 
type Stack = [Int]
type EvalM = State Stack

push :: Int -> EvalM ()
push x = modify (x:)

pop :: EvalM Int
pop = do
  stack <- get
  put (tail stack)
  return (head stack)

eval :: String -> Int
eval expr =
    evalState go []
  where
    go = mapM_ step (words expr) >> pop
    step "+" = binOp (+)
    step "-" = binOp (-)
    step "*" = binOp (*)
    step t = push (read t)
    binOp op = do
      x <- pop
      y <- pop
      push (op y x)

main :: IO ()
main = do 
  print $ eval "1 2 +"         -- 3
  print $ eval "1 2 *"         -- 2
  print $ eval "1 2 -"         -- -1
  print $ eval "1 2 + 3 * 4 -" -- 5 

-- ghci> eval "a 2 +"
-- *** Exception: Prelude.read: no parse
-- ghci> eval "2 +"
-- *** Exception: Prelude.head: empty list
-- ghci> eval ""
-- ghci> eval "- 1 2"
-- 2
-- ghci> eval "1 2 3 -"
-- -1
