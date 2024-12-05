module PythCont () where

-- Use this monad
import Control.Monad.Trans.Cont (Cont)

-- Rewrite this functions in CPS using Cont monad

-- addCps :: Int -> Int -> ((Int -> r) -> r)
-- addCps x y = \k -> k (x + y)

-- squareCps :: Int -> ((Int -> r) -> r)
-- squareCps x = \k -> k (x * x)

-- pythagorasCps :: Int -> Int -> ((Int -> r) -> r)
-- pythagorasCps x y = \k ->
--   squareCps x $ \x_squared ->
--     squareCps y $ \y_squared ->
--       addCps x_squared y_squared $ k

-- run :: ((r -> r) -> r) -> r
-- run f = f id
