{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module FuncDep.Matrix where

-- 2 * 2 matrices
 
data Vector = V Int Int deriving (Eq, Show)
data Matrix = M Vector Vector deriving (Eq, Show)

-- You can multiply 2 vectors, 2 matricies, an integer and a matrix, a matrix and a vector... 
-- We want to use one operator for all operations, so we create a type class: 

class Mult a b c | a b -> c where 
  (***) :: a -> b -> c 

instance Mult Vector Vector Int where 
  (V a b) *** (V c d) = a * c + b * d 

instance Mult Int Vector Vector where 
  x *** (V a b) = V (x * a) (x * b)

instance Mult Int Matrix Matrix where 
  x *** (M v w) = M (x *** v) (x *** w)

instance Mult Matrix Matrix Matrix where 
  (M (V a b) (V c d)) *** (M (V x y) (V z t)) = 
    M (V (a * x + c * y) (b * x + d * y)) (V (a * z + c * t) (b * z + d * t))

-- instance Mult Matrix Matrix Int where
--   (M (V a b) (V c d)) *** (M (V x y) (V z t)) = 
--     42

instance Mult Matrix Vector Vector where 
  (M (V a b) (V c d)) *** (V x y) = 
    V (a * x + c * y) (b * x + d * y)

v1 = V 1 3 
v2 = V 2 4

v3 = V 5 7
v4 = V 6 8

-- Haskell fails to typecheck v1 *** v2 

