{-# LANGUAGE TemplateHaskell #-}

module Examples where

import C2_CurryExercise
import Logging

$(makeUncurry 3)
f :: Int -> Int -> Int -> Int
f x y z = x + y + z

f' :: (Int, Int, Int) -> Int
f' = uncurry3 f

g :: Int -> Int -> Int
g x y = x + y
h :: String -> Int
h = length
$(generateLoggingFunctions ['g, 'h])

g' :: Int -> Int -> IO Int
g' = log_g
h' :: [Char] -> IO Int
h' = log_h