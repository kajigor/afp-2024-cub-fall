{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
-- {-# OPTIONS_GHC -fplugin=LiquidHaskell #-}
-- {-@ LIQUID "--no-termination" @-}

module Mx where

{-@ type BList N a = {l:[a] | len l == N } @-}
{-@ type Nat = {i: Int | i >= 0} @-}

{-@ data Vector a = Vec { n :: Nat, elems :: BList a n} @-}
data Vector a = Vec { n :: Int, elems :: [a] } deriving (Show)

{-@ type SizedVector N a = {v: Vector a | n v == N} @-}

{-@ data Matrix a = Mat { h :: Nat, w :: Nat, rows :: SizedVector (SizedVector a w) h} @-}
data Matrix a = Mat { h :: Int, w :: Int, rows :: Vector ( Vector a ) } deriving (Show)

{-@ !!! :: x:Vector a -> {v:Int | 0 <= v && v < n x} -> a @-}
(!!!) :: Vector a -> Int -> a
(!!!) (Vec n xs) i = xs !! i

{-@ ind :: m:Matrix a -> {i:Nat | i < h m} -> {j:Nat | j < w m} -> a @-}
ind :: Matrix a -> Int -> Int -> a
ind (Mat h w xs) i j = (xs !!! i) !!! j

{-@ col :: {m:Matrix a | h m > 0} -> {i:Nat | w m > i} -> {v:Vector a | h m == n v} @-}
col :: Matrix a -> Int -> Vector a
col m@(Mat h w xs) ix = Vec h $ go m ix
    where
        {-@ go :: {v:Matrix a | h v > 0} -> {i:Nat | w v > i} -> [a] @-}
        go :: Matrix a -> Int -> [a]
        go m@(Mat h w xs) colx = ind m 0 colx  : go m colx


impossible = undefined

{-@ vmul :: v1:Vector Int -> v2:Vector Int -> Int @-}
vmul :: Vector Int -> Vector Int -> Int
vmul (Vec _ []) (Vec _ []) = 0
vmul (Vec _ []) _ = impossible
vmul _ (Vec _ []) = impossible
vmul (Vec n (x : xs)) ( Vec m (y : ys)) = x * y + (Vec (n-1) xs `vmul` Vec (m-1) ys)

vplus :: Vector Int -> Vector Int -> Vector Int
vplus (Vec n v1) (Vec _ v2) = Vec n $ go v1 v2
  where
      go :: [Int] -> [Int] -> [Int]
      go (x:xs) (y:ys) = x + y : go xs ys
      go [] [] = []
      go [] _ = impossible
      go _ [] = impossible

(***) :: Matrix Int -> Matrix Int -> Matrix Int
(***) (Mat h w (Vec _ rows)) m2@(Mat h2 w2 _) = Mat h w2 $ Vec h $ go rows m2
  where
      go :: [Vector Int] -> Matrix Int -> [Vector Int]
      go (x:xs) m@(Mat h w ms) = Vec w (rowxcol x 0 m) : go xs m
      go [] _ = []

      rowxcol :: Vector Int -> Int -> Matrix Int -> [Int]
      rowxcol x ix m@(Mat h w v) = if w > ix
            then x `vmul` col m ix : rowxcol x (ix + 1) m
            else []

(+++) :: Matrix Int -> Matrix Int -> Matrix Int
(+++) (Mat h w (Vec _ r1)) (Mat h2 w2 (Vec _ r2)) = Mat h w $ Vec h $ go r1 r2
  where
      go :: [Vector Int] -> [Vector Int] -> [Vector Int]
      go (x:xs) (y:ys) = (x `vplus` y) : go xs ys
      go _ [] = []
      go [] _ = []

v1 :: Vector Int
v1 = Vec 4 [1, 2, 3, 4]
v2 :: Vector Int
v2 = Vec 4 [5, 6, 7, 8]
v3 :: Vector Int
v3 = v1 `vplus` v2
v4 :: Int
v4 = v1 `vmul` v2

m1 :: Matrix Int
m1 = Mat 2 3 $ Vec 2 [Vec 3 [1, 2, 3], Vec 3 [4, 5, 6]]
m2 :: Matrix Int
m2 = Mat 2 3 $ Vec 2 [Vec 3 [-1, -2, -3], Vec 3 [-4, -5, -6]]
m3 :: Matrix Int
m3 = m1 +++ m2

m4 :: Matrix Int
m4 = Mat 3 2 $ Vec 3 [Vec 2 [10, 20], Vec 2 [30, 40], Vec 2 [50, 60]]
m5 :: Matrix Int
m5 = m1 *** m4

main :: IO ()
main = do
    print v3
    print v4
    print $ col m4 0
    print $ col m4 1
    print m3
    print m5