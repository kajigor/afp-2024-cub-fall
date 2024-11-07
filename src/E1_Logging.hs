{-# LANGUAGE TemplateHaskell #-}

module E1_LoggingExercise where

import Language.Haskell.TH
import E2_LoggingExercise
import Control.Monad (replicateM)
import Text.Read (readMaybe)

test1 :: Int -> Int -> String
test1 a b = show a ++ " " ++ show b

test2 :: Int -> Int -> Int
test2 a b = a + b

test3 :: Int -> Maybe Int -> Int
test3 a b = case b of 
    Just b -> a + b 
    Nothing -> a 

$(generateLoggingFunctions ['test1, 'test2, 'test3])
