{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
module PythCps () where

add :: Int -> Int -> Int
add x y = x + y

square :: Int -> Int
square x = x * x

pythagoras :: Int -> Int -> Int
pythagoras x y = add (square x) (square y)

addCps :: Int -> Int -> ((Int -> r) -> r)
addCps x y = \k -> k (add x y)

squareCps :: Int -> ((Int -> r) -> r)
squareCps x = \k -> k (square x)

pythagorasCps :: Int -> Int -> ((Int -> r) -> r)
pythagorasCps x y = \k ->
  squareCps x $ \x_squared ->
    squareCps y $ \y_squared ->
      addCps x_squared y_squared $ k

divCps :: Int -> Int -> ((Int -> String) -> String)
divCps x 0 = \k -> "Division by zero"
divCps x y = \k -> k (x `div` y)

-- x^2 / c + y^2
pythagorasCps' :: Int -> Int -> Int -> ((Int -> String) -> String)
pythagorasCps' x y c = \k ->
  squareCps x $ \x_squared ->
    divCps x_squared c $ \x_squared_divided ->
      squareCps y $ \y_squared ->
        addCps x_squared_divided y_squared $ k

run :: ((Int -> String) -> String) -> String
run f = f show
