module Serialization.Final where

import qualified ExpressionProblem.Final as M
import qualified Intro.Final as E
import Text.Printf (printf)
import qualified ExpressionProblem.InitialMul as E

data Json
  = Leaf String
  | Node String [Json]
  deriving (Show)

json :: Json
json = Node "Mul" [ Node "Neg" [ Node "Lit" [Leaf "13"] ]
                  , Node "Mul" [ Node "Lit" [Leaf "42"]
                               , Node "Lit" [Leaf "777"] ] ]

-- mul (neg (lit 13)) (mul (lit 42) (lit 777))

instance E.ExprSYM Json where 
  lit n = Node "Lit" [ Leaf $ show n ]
  neg e = Node "Neg" [ e ]
  add x y = Node "Add" [ x, y ]

instance M.MulSYM Json where 
  mul x y = Node "Mul" [x, y]

toJson :: Json -> Json 
toJson = id 

main :: IO ()
main = do
  print $ toJson M.expr -- Node "Mul" [Node "Neg" [Node "Lit" [Leaf "13"]],Node "Mul" [Node "Lit" [Leaf "42"],Node "Lit" [Leaf "777"]]]

type Err = String 

safeRead :: (Read b) => String -> Either Err b
safeRead s =
  case reads s of
    [(x, "")] -> Right x
    _ -> Left $ printf "Read error: %s" s

fromTree :: (E.ExprSYM a, M.MulSYM a) => Json -> Either Err a
fromTree (Node "Lit" [Leaf n]) = do 
  n <- safeRead n 
  return $ E.lit n 
