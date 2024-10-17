{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Intro.Final where

import Text.Printf ( printf )

-- data Expr
--   = Lit Int
--   | Neg Expr
--   | Add Expr Expr

class ExprSYM repr where 
  lit :: Int -> repr 
  neg :: repr -> repr 
  add :: repr -> repr -> repr 

instance ExprSYM Int where 
  lit n = n 
  neg e = - e 
  add x y = x + y 

instance ExprSYM String where 
  lit n = show n 
  neg e = printf "(- %s)" e 
  add x y = printf "(%s + %s)" x y 

newtype Prefix = Prefix { getPrefix :: String }

instance ExprSYM Prefix where 
  lit = Prefix . show 
  neg e = Prefix $ printf "- %s" (getPrefix e)
  add x y = Prefix $ printf "+ %s %s" (getPrefix x) (getPrefix y)

prefix :: Prefix -> String 
prefix = getPrefix 

expr :: ExprSYM a => a 
expr = add (neg (lit 13)) (lit 42)

view :: String -> String 
view = id 

eval :: Int -> Int 
eval = id 

main :: IO ()
main = do 
  putStrLn $ view expr 
  print $ eval expr 
  putStrLn $ prefix expr 


