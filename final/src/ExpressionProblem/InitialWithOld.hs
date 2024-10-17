module ExpressionProblem.InitialWithOld where

import qualified Intro.Initial as I
import Text.Printf (printf)

data Expr
  = Old I.Expr
  | Mul Expr Expr

eval :: Expr -> Int
eval (Mul x y) = eval x * eval y
eval (Old e) = I.eval e

view :: Expr -> String
view (Mul x y) = printf "(%s * %s)" (view x) (view y)
view (Old e) = I.view e

expr :: Expr
expr = Mul (Old (I.Neg (I.Lit 13))) (Old (I.Add (I.Lit 42) (I.Lit 777)))

main :: IO ()
main = do
  putStrLn $ view expr
  print $ eval expr