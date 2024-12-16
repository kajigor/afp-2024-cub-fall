module Main (main) where

import Interpreter (runExpr, ReturnValue, Error)
import ExprParser (parseExpression)

main :: IO ()
main = do
  print $ runWhole example1
  print $ runWhole example2
  print $ runWhole example
  print $ runWhole example3

example :: String
example = "x := sin(1.57) + 1;y := 5 / (tg(1) + (3 * 4));y-x"

example1 :: String
example1 = "1"

example2 :: String
example2 = "1 + 2"

example3 :: String
example3 = "x:=1 + 2;x"

runWhole :: String -> Maybe (Either Error ReturnValue)
runWhole s = runExpr <$> parseExpression s
