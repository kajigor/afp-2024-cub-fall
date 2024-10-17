{-# LANGUAGE GADTs #-}

module Initial where

import Common
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import Text.Printf (printf)

data Dsl env t where
  IntConst :: Int -> Dsl env Int
  BoolConst :: Bool -> Dsl env Bool
  
  IntBin :: BinOp (Int -> Int -> Int) -> Dsl env Int -> Dsl env Int -> Dsl env Int
  BoolBin :: BinOp (Bool -> Bool -> Bool) -> Dsl env Bool -> Dsl env Bool -> Dsl env Bool
  Compare :: BinOp (Int -> Int -> Bool) -> Dsl env Int -> Dsl env Int -> Dsl env Bool
  IfE :: Dsl env Bool -> Dsl env t -> Dsl env t -> Dsl env t
  -- HOAS higher order abstract syntax 
  Var :: env t -> Dsl env t
  Lam :: (Dsl env t1 -> Dsl env t2) -> Dsl env (t1 -> t2)
  App :: Dsl env (t1 -> t2) -> Dsl env t1 -> Dsl env t2
  Fix :: (Dsl env t -> Dsl env t) -> Dsl env t

eval :: Dsl Env t -> t
eval (IntConst n) = n
eval (BoolConst b) = b
eval (IntBin f x y) = getOp f (eval x) (eval y)
eval (BoolBin f x y) = getOp f (eval x) (eval y)
eval (Compare f x y) = getOp f (eval x) (eval y)
eval (IfE cond thn els) = if eval cond then eval thn else eval els
eval (Var v) = unEnv v
eval (Lam f) = \x -> eval (f (Var $ Env x))
eval (App x y) = eval x $ eval y
eval (Fix f) = eval (f (Fix f))

view' :: Dsl S t -> State VarState String
view' (IntConst n) = return $ show n
view' (BoolConst b) = return $ show b
view' (IntBin op x y) = do
  x <- view' x
  y <- view' y
  return $ formatBinOp op x y
view' (BoolBin op x y) = do
  x <- view' x
  y <- view' y
  return $ formatBinOp op x y
view' (Compare op x y) = do
  x <- view' x
  y <- view' y
  return $ formatBinOp op x y
view' (IfE cond thn els) = do
  cond <- view' cond
  thn <- view' thn
  els <- view' els
  return $ printf "if %s then %s else %s" cond thn els
view' (Var v) = unS v
view' (Lam f) = do
  v <- newVar
  let var = printf "x.%s" (show v) :: String
  x <- view' (f (Var . S $ return var))
  return $ printf "(\\%s -> %s)" var x
view' (App x y) = do
  x <- view' x
  y <- view' y
  return $ printf "(%s) (%s)" x y
view' (Fix f) = do
  v <- newVar
  let self = printf "self.%s" (show v) :: String
  b <- view' (f (Var . S $ return self))
  return $ printf "(fix %s . %s)" self b

view :: (Show t) => Dsl S t -> String
view e = evalState (view' e) (VarState 0)

andOp :: Dsl env Bool -> Dsl env Bool -> Dsl env Bool
andOp = BoolBin (BinOp "&&" (&&))

mulOp :: Dsl env Int -> Dsl env Int -> Dsl env Int
mulOp = IntBin (BinOp "*" (*))

addOp :: Dsl env Int -> Dsl env Int -> Dsl env Int
addOp = IntBin (BinOp "+" (+))

leqOp :: Dsl env Int -> Dsl env Int -> Dsl env Bool
leqOp = Compare (BinOp "<=" (<=))

-- x^n
tipow :: Dsl env (Int -> Int -> Int)
tipow =
  Lam
    ( \x ->
        Fix
          ( \self ->
              Lam
                ( \n ->
                    IfE
                      (leqOp n (IntConst 0))
                      (IntConst 1)
                      (mulOp x (App self (addOp n (IntConst (-1)))))
                )
          )
    )

-- tipowApplied :: Int -> Int -> Dsl env Int
tipowApplied x y = App (App tipow (IntConst x)) (IntConst y)

-- pair = \f.\s.\b.b f s
pair = Lam $ \f -> Lam $ \s -> Lam $ \b -> App (App b f) s

-- frst = \p. p tru
frst = Lam $ \p -> App p tru

-- tru = \t.\f.t
tru = Lam $ \t -> Lam $ \f -> t

fstOfPair :: Dsl env (Int -> Bool -> Int)
fstOfPair = Lam $ \u -> Lam $ \w -> App frst (App (App pair u) w)

fstOfPairApplied :: Int -> Bool -> Dsl env Int
fstOfPairApplied u w = App (App fstOfPair (IntConst u)) (BoolConst w)

-- eval (fstOfPair (Var $ Env "y") (Var $ Env "x"))
-- view fstOfPairApplied