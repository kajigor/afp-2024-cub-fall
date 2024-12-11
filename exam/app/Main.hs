module Main (main) where

import Interpreter (runExpr)
import ExprParser (parseExpression)

main :: IO ()
main = print $ runWhole example

example = "x := sin(1.57) + 1;y := 5 / (1 + (3 * 4));y-x"

runWhole s = runExpr <$> parseExpression s
