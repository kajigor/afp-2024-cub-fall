module Intro.FinalBad where

import Text.Printf ( printf )

-- data Expr
--   = Lit Int
--   | Neg Expr
--   | Add Expr Expr

-- shallow embedding == combinators 

type Repr = Int 

lit :: Int -> Repr 
lit n = n 

neg :: Repr -> Repr 
neg x = - x 

add :: Repr -> Repr -> Repr 
add x y = x + y 

expr :: Repr
expr = add (neg (lit 13)) (lit 42)
 
main :: IO ()
main = do 
  print expr 
