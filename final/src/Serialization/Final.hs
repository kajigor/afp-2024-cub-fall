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

-- main :: IO ()
-- main = do
--   print $ toJson M.expr -- Node "Mul" [Node "Neg" [Node "Lit" [Leaf "13"]],Node "Mul" [Node "Lit" [Leaf "42"],Node "Lit" [Leaf "777"]]]

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
fromTree (Node "Neg" [x]) = do
  x <- fromTree x
  return $ E.neg x
fromTree (Node "Add" [x, y]) = do
  x <- fromTree x
  y <- fromTree y
  return $ E.add x y
fromTree (Node "Mul" [x, y]) = do
  x <- fromTree x
  y <- fromTree y
  return $ M.mul x y
fromTree e =
  Left $ printf "Invalid tree: %s" (show e)

fromJsonExt ::
  (E.ExprSYM repr, M.MulSYM repr) =>
  (Json -> Either Err repr) ->
  Json ->
  Either Err repr
fromJsonExt self (Node "Lit" [Leaf n]) = do 
  n <- safeRead n 
  return $ E.lit n 
fromJsonExt self (Node "Neg" [e]) = do 
  e <- self e 
  return $ E.neg e 
fromJsonExt self (Node "Add" [x, y]) = do 
  x <- self x 
  y <- self y 
  return $ E.add x y 
fromJsonExt self (Node "Mul" [x, y]) = do
  x <- self x
  y <- self y
  return $ M.mul x y
fromJsonExt self e = Left $ "Invalid tree: " ++ show e

fix f = f (fix f) -- -> f (f (f ... ))

fromJson' = fix fromJsonExt

doStuff = checkConsume thrice . fromJson'

json' = Node "Lit" [Leaf "13"]

instance (E.ExprSYM repr, E.ExprSYM repr') => E.ExprSYM (repr, repr') where
  lit x = (E.lit x, E.lit x)
  neg (e1, e2) = (E.neg e1, E.neg e2)
  add (e11, e12) (e21, e22) = (E.add e11 e21, E.add e12 e22)

instance (M.MulSYM repr, M.MulSYM repr') => M.MulSYM (repr, repr') where
  mul (e11, e12) (e21, e22) = (M.mul e11 e21, M.mul e12 e22)

duplicate ::
  (E.ExprSYM repr, E.ExprSYM repr', M.MulSYM repr, M.MulSYM repr') =>
  (repr, repr') ->
  (repr, repr')
duplicate = id

checkConsume :: (t -> IO ()) -> Either [Char] t -> IO ()
checkConsume f (Left  e) = putStrLn $ "Error: " ++ e
checkConsume f (Right x) = f x

dupConsume :: (Show a, E.ExprSYM t, E.ExprSYM b, M.MulSYM t, M.MulSYM b) => (t -> a) -> (t, b) -> IO b
dupConsume ev x = print (ev x1) >> return x2
 where (x1,x2) = duplicate x

thrice :: (Int, (String, Json)) -> IO ()
thrice x = dupConsume M.eval x >>= dupConsume M.view >>= print . toJson

invalidJson =  Node "Lit" [Leaf "1", Leaf "2"]

main :: IO ()
main = do
  print $ toJson M.expr -- Node "Mul" [Node "Neg" [Node "Lit" [Leaf "13"]],Node "Mul" [Node "Lit" [Leaf "42"],Node "Lit" [Leaf "777"]]]
  case fromTree invalidJson of
    Right t -> do
      doStuff t 
    Left err -> putStrLn err