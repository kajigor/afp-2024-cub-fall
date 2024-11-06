{-# LANGUAGE TemplateHaskell #-}
module E1_Logged where 

import E2_LoggedExercise

test1 :: Int -> Int -> Int
test1 a b = a + b

test2 :: Int -> (Int, Int, Int)
test2 a = (1, a, 2 * a)

$(generateLoggingFunctions ['test1, 'test2])

main :: IO ()
main = test1Logged 3 4 >> test2Logged 2 >> return ()