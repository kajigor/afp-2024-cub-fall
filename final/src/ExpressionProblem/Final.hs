{-# LANGUAGE FlexibleInstances #-}
module ExpressionProblem.Final where 

import qualified Intro.Final as F
import Text.Printf (printf)

-- data Expr
--   = Lit Int
--   | Neg Expr
--   | Add Expr Expr
--   | Mul Expr Expr

class MulSYM repr where
  mul :: repr -> repr -> repr

instance MulSYM Int where
  mul x y = x * y

instance MulSYM String where
  mul x y = printf "(%s * %s)" x y

eval :: Int -> Int
eval = id

view :: String -> String
view = id

expr :: (MulSYM a, F.ExprSYM a) => a
expr = mul (F.neg (F.lit 13)) (mul (F.lit 42) (F.lit 777))

main :: IO ()
main = do
  putStrLn $ view expr
  print $ eval expr
