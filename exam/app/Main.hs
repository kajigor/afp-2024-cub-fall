module Main (main) where

import Interpreter (runExpr, ReturnValue, Error)
import ExprParser (parseExpression)

main :: IO ()
main = print $ runWhole example

example :: String
example = "x := sin(1.57) + 1;y := 5 / (tg(1) + (3 * 4));y-x"

runWhole :: String -> Maybe (Either Error ReturnValue)
runWhole s = runExpr <$> parseExpression s
