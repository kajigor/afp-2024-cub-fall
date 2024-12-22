{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fplugin=LiquidHaskell #-}
{-@ LIQUID "--no-termination" @-}

module Lh where

import LiquidHaskell

{-@ type BList a N = {l:[a] | len l == N } @-}
{-@ type Nat = {i: Int | i >= 0} @-}

{-@ data Vector a = Vec {mLen :: Int, elems :: BList a mLen} @-}
data Vector a = Vec {mLen :: Int, elems :: [a] } deriving (Show)
{-@ type VectorN a N = {v:Vector a | mLen v == N} @-}

{-@ data Matrix a = Mat { mRow :: Nat, mCol :: Nat, rows :: VectorN (VectorN a mCol) mRow} @-}
data Matrix a = Mat { mRow :: Int, mCol :: Int, rows :: Vector ( Vector a ) } deriving (Show)

{-@ type NEMatrix a = {m:Matrix a | mRow m > 0 && mCol m > 0} @-}
{-@ type MatrixN a R C = {m:Matrix a | mRow m == R && mCol m == C} @-}

{-@ !!! :: {x:Vector a | mLen x > 0} -> {v:Int | 0 <= v && v < mLen x} -> a @-}
(!!!) :: Vector a -> Int -> a
(!!!) (Vec _ xs) i = xs !! i

{-@ ind :: m:NEMatrix a
         -> {i:Int | 0 <= i && i < mRow m} 
         -> {j:Int | 0 <= j && j < mCol m} 
         -> a @-}
ind :: Matrix a -> Int -> Int -> a
ind (Mat _ _ xs) i j = (xs !!! i) !!! j

{-@ for :: v:Vector a -> (a -> b) -> {v1:Vector b | mLen v1 == mLen v} @-}
for :: Vector a -> (a -> b) -> Vector b
for (Vec n xs) f = Vec n (map f xs)

{-@ impossible :: {v:String | false} -> a @-}
impossible msg = error msg

{-@ vmul :: v1:Vector Int -> {v2:Vector Int | mLen v1 == mLen v2} -> Int @-}
vmul :: Vector Int -> Vector Int -> Int
vmul (Vec 0 _) (Vec 0 _) = 0
vmul (Vec _ []) _ = impossible "vectors of different length"
vmul _ (Vec _ []) = impossible "vectors of different length"
vmul (Vec n (x : xs)) ( Vec m (y : ys)) = x * y + (Vec (n-1) xs `vmul` Vec (m-1) ys)

{-@ vplus :: v1:Vector Int -> {v2:Vector Int | mLen v1 == mLen v2} -> {v3:Vector Int | mLen v3 == mLen v1}@-}
vplus :: Vector Int -> Vector Int -> Vector Int
vplus (Vec n v1) (Vec m v2) = Vec n $ go v1 v2
  where
      go :: [Int] -> [Int] -> [Int]
      go (x:xs) (y:ys) = x + y : go xs ys
      go [] [] = []
      go [] _ = impossible "vectors of different length"
      go _ [] = impossible "vectors of different length"

{-@ (***) :: x:Matrix Int -> {y:Matrix Int | mCol x == mRow y} -> MatrixN Int (mRow x) (mCol y) @-}
(***) :: Matrix Int -> Matrix Int -> Matrix Int
(***) (Mat rws _ xs) m@(Mat _ cls _) = Mat rws cls $ for xs (\xi -> for (rows (transpose m)) (\yi -> xi `vmul` yi))
  where
      {-@ transpose :: m:Matrix a -> MatrixN a (mCol m) (mRow m) @-}
      transpose :: Matrix a -> Matrix a
      transpose (Mat r c rows) = Mat c r $ go c r rows

      {-@ go :: cx:Nat -> rx:Nat -> VectorN (VectorN a cx) rx -> VectorN (VectorN a rx) cx @-}
      go :: Int -> Int -> Vector (Vector a) -> Vector (Vector a)
      go 0 _ _ = Vec 0 []
      go cx rx (Vec rx2 rows) = let heads = map (\(Vec _ r) -> head r) rows
                                    tails = map (\(Vec _ r) -> Vec (cx - 1) $ tail r) rows
                                    (Vec _ vTails) = go (cx-1) rx (Vec rx tails)
                                    in Vec cx $ Vec rx heads : vTails 


{-@ (+++) :: m1:Matrix Int -> {m2:Matrix Int | mRow m1 == mRow m2 && mCol m1 == mCol m2} -> {m3:Matrix Int | mRow m3 == mRow m2 && mCol m3 == mCol m2} @-}
(+++) :: Matrix Int -> Matrix Int -> Matrix Int
(+++) (Mat h1 w1 (Vec _ r1)) (Mat h2 w2 (Vec _ r2)) = Mat h1 w1 $ Vec h1 $ go r1 r2
  where
      go :: [Vector Int] -> [Vector Int] -> [Vector Int]
      go (x:xs) (y:ys) = (x `vplus` y) : go xs ys
      go _ [] = []
      go [] _ = []

{-@ v1 :: VectorN _ 4 @-}
v1 :: Vector Int
v1 = Vec 4 [1, 2, 3, 4]
{-@ v2 :: VectorN _ 4 @-}
v2 :: Vector Int
v2 = Vec 4 [5, 6, 7, 8]
{-@ v3 :: VectorN _ 4 @-}
v3 :: Vector Int
v3 = v1 `vplus` v2
v4 :: Int
v4 = v1 `vmul` v2

-- v5 = Vec 10 [1, 2] -- fails, list size does not match declared size
v6 = Vec 1 [1]
-- v7 = v1 `vplus` v6 -- fails, dim mismatch
-- v8 = v1 `vmul` v6 -- fails, dim mismatch

{-@ m1 :: MatrixN _ 2 3 @-}
m1 :: Matrix Int
m1 = Mat 2 3 $ Vec 2 [Vec 3 [1, 2, 3], Vec 3 [4, 5, 6]]
{-@ m2 :: MatrixN _ 2 3 @-}
m2 :: Matrix Int
m2 = Mat 2 3 $ Vec 2 [Vec 3 [-1, -2, -3], Vec 3 [-4, -5, -6]]
{-@ m3 :: MatrixN _ 2 3 @-}
m3 :: Matrix Int
m3 = m1 +++ m2

{-@ m4 :: MatrixN _ 3 2 @-}
m4 :: Matrix Int
m4 = Mat 3 2 $ Vec 3 [Vec 2 [10, 20], Vec 2 [30, 40], Vec 2 [50, 60]]
m5 :: Matrix Int
{-@ m5 :: MatrixN _ 2 2 @-}
m5 = m1 *** m4

-- m6 = m1 +++ m4 -- fails
-- m7 = m1 *** m2 -- fails
-- m8 = Mat 2 2 $ Vec 0 [] -- fails, vec size mismatch

main :: IO ()
main = do
    print v3
    print v4
    print m3
    print m5