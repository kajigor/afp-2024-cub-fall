module ExpressionProblem.InitialMul where

import Text.Printf (printf)

data Expr
  = Lit Int
  | Neg Expr
  | Add Expr Expr
  | Mul Expr Expr

expr :: Expr
expr = Mul (Neg (Lit 13)) (Lit 42)

eval :: Expr -> Int
eval (Lit n) = n
eval (Neg e) = -(eval e)
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

view :: Expr -> String
view (Lit n) = show n
view (Neg e) = printf "(- %s)" (view e)
view (Add x y) = printf "(%s + %s)" (view x) (view y)
view (Mul x y) = printf "(%s * %s)" (view x) (view y)

main :: IO ()
main = do
  putStrLn $ view expr
  print $ eval expr