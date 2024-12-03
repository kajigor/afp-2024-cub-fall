{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE InstanceSigs #-}
module CPS_clean () where
import Control.Monad (when)
import Data.Char (digitToInt, intToDigit)
import Control.Monad.Trans.Class (lift)
import Data.Functor.Identity (Identity (runIdentity))

-- sqrtCps :: Int -> ((Int -> String) -> String)
-- sqrtCps x | x >= 0 = \k -> k ((floor . sqrt . fromIntegral) x)
--           | x < 0 = \k -> show ""

-- (3^2 + 4^2) / 5 + 1
-- correctCps ::((Int -> String) -> String)
-- correctCps k = pythagorasCps 3 4 $ \pythagoras -> 
--   divCps pythagoras 5 $ \pythagoras_divided -> 
--     addCps pythagoras_divided 1 k

-- (3^2 + 4^2) / 0 + 1
-- incorrectCps ::((Int -> String) -> String)
-- incorrectCps k = pythagorasCps 3 4 $ \pythagoras -> 
--   divCps pythagoras 0 $ \pythagoras_divided -> 
--     addCps pythagoras_divided 1 k


addCps :: Int -> Int -> ((Int -> String) -> String)
addCps x y k = k (x + y)

squareCps :: Int -> ((Int -> String) -> String)
squareCps x k = k (x * x)

pythagorasCps :: Int -> Int -> ((Int -> String) -> String)
pythagorasCps x y k =
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

runCps :: ((Int -> String) -> String) -> String
runCps f = f show

-- runCps :: ((r -> r) -> r) -> r
-- runCps f = f id

chainCPS ::
  ((a -> r) -> r) ->
  (a -> ((b -> r) -> r)) ->
  ((b -> r) -> r)
chainCPS s f = \k -> s $ \x -> f x $ k

