{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
-- {-# OPTIONS_GHC -fplugin=LiquidHaskell #-}
-- {-@ LIQUID "--no-termination" @-}

module Mx where

data Vector a = Vec {  elems :: [a] } deriving (Show)

data Matrix a = Mat { rows :: Vector ( Vector a ) } deriving (Show)
(!!!) :: Vector a -> Int -> a
(!!!) (Vec xs) i = xs !! i

ind :: Int -> Int -> Matrix a -> a
ind i j (Mat xs) = (xs !!! i) !!! j

col :: Int -> Matrix a -> Vector a
col ix (Mat xs) = Vec $ go xs ix
    where
        go :: Vector (Vector a) -> Int -> [a]
        go (Vec []) _ = []
        go v@(Vec (_:rows)) colx = ind 0 colx (Mat v) : go (Vec rows) colx


impossible = undefined

{-@ vmul :: v1:Vector Int -> v2:Vector Int -> Int @-}
vmul :: Vector Int -> Vector Int -> Int
vmul (Vec []) (Vec []) = 0
vmul (Vec []) _ = impossible
vmul _ (Vec []) = impossible
vmul (Vec (x : xs)) ( Vec (y : ys)) = x * y + (Vec xs `vmul` Vec ys)

vplus :: Vector Int -> Vector Int -> Vector Int
vplus (Vec v1) (Vec v2) = Vec $ go v1 v2
  where
      go :: [Int] -> [Int] -> [Int]
      go (x:xs) (y:ys) = x + y : go xs ys
      go [] [] = []
      go [] _ = impossible
      go _ [] = impossible

(***) :: Matrix Int -> Matrix Int -> Matrix Int
(***) (Mat (Vec rows)) m2@(Mat _) = Mat $ Vec $ go rows m2
  where
      go :: [Vector Int] -> Matrix Int -> [Vector Int]
      go (x:xs) m@(Mat ms) = Vec (rowxcol x 0 m) : go xs m
      go [] _ = []

      rowxcol :: Vector Int -> Int -> Matrix Int -> [Int]
      rowxcol x ix m@(Mat (Vec ((Vec v):vs))) = if length v > ix
            then x `vmul` col ix m : rowxcol x (ix + 1) m
            else []

(+++) :: Matrix Int -> Matrix Int -> Matrix Int
(+++) (Mat (Vec r1)) (Mat (Vec r2)) = Mat $ Vec $ go r1 r2
  where
      go :: [Vector Int] -> [Vector Int] -> [Vector Int]
      go (x:xs) (y:ys) = (x `vplus` y) : go xs ys
      go _ [] = []
      go [] _ = []

v1 :: Vector Int
v1 = Vec [1, 2, 3, 4]
v2 :: Vector Int
v2 = Vec [5, 6, 7, 8]
v3 :: Vector Int
v3 = v1 `vplus` v2
v4 :: Int
v4 = v1 `vmul` v2

m1 :: Matrix Int
m1 = Mat $ Vec [Vec [1, 2, 3], Vec [4, 5, 6]]
m2 :: Matrix Int
m2 = Mat $ Vec [Vec [-1, -2, -3], Vec [-4, -5, -6]]
m3 :: Matrix Int
m3 = m1 +++ m2

m4 :: Matrix Int
m4 = Mat $ Vec [Vec [10, 20], Vec [30, 40], Vec [50, 60]]
m5 :: Matrix Int
m5 = m1 *** m4

main :: IO ()
main = do
    print v3
    print v4
    print $ col 0 m4
    print $ col 1 m4 
    print m3
    print m5