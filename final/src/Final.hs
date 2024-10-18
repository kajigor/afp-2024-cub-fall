{-# LANGUAGE NoMonomorphismRestriction #-}

module Final where

import Common
import Control.Monad.State hiding (fix)
import Text.Printf
import Prelude hiding (compare)
import qualified Data.Function

-- Finally tagless, partially evaluated
-- Oleg Kiselyov

-- data Dsl env t where
--   IntConst :: Int -> Dsl env Int
--   BoolConst :: Bool -> Dsl env Bool

--   IntBin  :: BinOp (Int -> Int -> Int)    -> Dsl env Int  -> Dsl env Int  -> Dsl env Int
--   BoolBin :: BinOp (Bool -> Bool -> Bool) -> Dsl env Bool -> Dsl env Bool -> Dsl env Bool
--   Compare :: BinOp (Int -> Int -> Bool)   -> Dsl env Int  -> Dsl env Int  -> Dsl env Bool
--   IfE :: Dsl env Bool -> Dsl env t -> Dsl env t -> Dsl env t

--   Var :: env t -> Dsl env t
--   Lam :: (Dsl env t1 -> Dsl env t2) -> Dsl env (t1 -> t2)
--   App :: Dsl env (t1 -> t2) -> Dsl env t1 -> Dsl env t2
--   Fix :: (Dsl env t -> Dsl env t) -> Dsl env t

newtype Box a = Box { unBox :: a }

class Calc repr where
  intConst :: Int -> repr Int
  intBin :: BinOp (Int -> Int -> Int) -> repr Int -> repr Int -> repr Int

class Cond repr where
  boolConst :: Bool -> repr Bool
  boolBin :: BinOp (Bool -> Bool -> Bool) -> repr Bool -> repr Bool -> repr Bool
  compare :: BinOp (Int -> Int -> Bool) -> repr Int -> repr Int -> repr Bool
  ifExpr :: repr Bool -> repr t -> repr t -> repr t

class Lam repr where
  lam :: (repr a -> repr b) -> repr (a -> b)
  app :: repr (a -> b) -> repr a -> repr b
  fix :: (repr a -> repr a) -> repr a

instance Calc Box where
  intConst = Box
  intBin binop l r = Box $ getOp binop (unBox l) (unBox r)

instance Cond Box where
  boolConst = Box
  boolBin binop l r = Box $ getOp binop (unBox l) (unBox r)
  compare binop l r = Box $ getOp binop (unBox l) (unBox r)
  ifExpr c t e = if unBox c then t else e

instance Lam Box where
  lam f = Box $ \x -> unBox (f (Box x))
  app f a = Box $ unBox f (unBox a)
  fix f = Box $ Data.Function.fix (unBox . f . Box)

instance Calc S where
  intConst n = S $ return $ show n
  intBin binop l r = S $ do 
    l <- unS l
    r <- unS r
    return $ formatBinOp binop l r

instance Cond S where
  boolConst b = S $ return $ show b
  boolBin binop l r = S $ do
    l <- unS l
    r <- unS r
    return $ formatBinOp binop l r
  compare binop l r = S $ do
    l <- unS l
    r <- unS r
    return $ formatBinOp binop l r
  ifExpr c t e = S $ do
    c <- unS c
    t <- unS t
    e <- unS e
    return $ printf "if %s then %s else %s fi" c t e

instance Lam S where
  lam f = S $ do
    v <- newVar
    let vName = printf "x%d" v :: String
    body <- unS (f (S $ return vName))
    return $ printf "(\\%s -> %s)" vName body
  app f a = S $ do
    f <- unS f
    a <- unS a
    return $ printf "(%s %s)" f a
  fix f = S $ do
    v <- newVar
    let vName  = printf "x%d" v :: String
    body <- unS (f (S $ return vName))
    return $ printf "(fix \\%s -> %s)" vName body

eval :: Box a -> a
eval = unBox

view :: S a -> String
view x = evalState (unS x) (VarState 0)

andOp :: (Cond repr) => repr Bool -> repr Bool -> repr Bool
andOp = boolBin (BinOp "&&" (&&))

mulOp :: (Calc repr) => repr Int -> repr Int -> repr Int
mulOp = intBin (BinOp "*" (*))

addOp :: (Calc repr) => repr Int -> repr Int -> repr Int
addOp = intBin (BinOp "+" (+))

leqOp :: (Cond repr) => repr Int -> repr Int -> repr Bool
leqOp = compare (BinOp "<=" (<=))

tipow :: (Cond repr, Calc repr, Lam repr) => repr (Int -> Int -> Int)
tipow =
  lam
    ( \x ->
        fix
          ( \self ->
              lam
                ( \n ->
                    ifExpr
                      (leqOp n (intConst 0))
                      (intConst 1)
                      (mulOp x (app self (addOp n (intConst (-1)))))
                )
          )
    )

tipowApplied :: (Cond repr, Calc repr, Lam repr) => Int -> Int -> repr Int
tipowApplied x y = app (app tipow (intConst x)) (intConst y)

main = do
  let expr = tipowApplied 4 2
  putStrLn $ view expr
  print $ eval expr
