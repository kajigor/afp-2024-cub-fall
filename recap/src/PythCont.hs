module PythCont () where

import Control.Monad.Trans.Cont (Cont, runCont, shift)

addCont :: Int -> Int -> Cont r Int
addCont x y = return (x + y)

squareCont :: Int -> Cont r Int
squareCont x = return (x * x)

pythagorasCont :: Int -> Int -> Cont r Int
pythagorasCont x y = do
  x_squared <- squareCont x
  y_squared <- squareCont y
  addCont x_squared y_squared

divCont :: Int -> Int -> Cont String Int
divCont x 0 = shift (\k -> return "Division by zero")
divCont x y = return (x `div` y)

-- x^2 / c + y^2
pythagorasCont' :: Int -> Int -> Int -> Cont String Int
pythagorasCont' x y c = do
  x_squared <- squareCont x
  x_squared_divided <- divCont x_squared c
  y_squared <- squareCont y
  addCont x_squared_divided y_squared

run :: Cont r r -> r
run m = runCont m id
