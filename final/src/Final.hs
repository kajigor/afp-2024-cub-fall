{-# LANGUAGE NoMonomorphismRestriction #-}

module Final where

import Common
import Control.Monad.State hiding (fix)
import Text.Printf
import Prelude hiding (compare)

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