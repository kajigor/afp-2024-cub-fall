{-# LANGUAGE TemplateHaskell #-}
module C1_Curry where 

import C2_CurryExercise

f2 :: (Int, Int) -> Int
f2 (a, b) = a + b

f2' :: Int -> Int -> Int
f2' = curry f2



f3 :: (Int, Int, Int) -> Int
f3 (a, b, c) = a + b + c

$(makeCurryUntyped 3)

f3' :: Int -> Int -> Int -> Int
f3' = curry3 f3

$(makeCurry 4)

f4 :: (Int, Int, Int, Int) -> Int
f4 (a, b, c, d) = a + b + c + d

f4' :: Int -> Int -> Int -> Int -> Int
f4' = curry4 f4

$(makeUncurry 4)

f4'' :: (Int, Int, Int, Int) -> Int
f4'' = uncurry4 f4'