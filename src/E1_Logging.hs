{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module E1_LoggingExercise where

import Language.Haskell.TH
import E2_LoggingExercise
import Control.Monad (replicateM)
import Text.Read (readMaybe)

test1 :: Int -> Int -> String
test1 a b = show a ++ " " ++ show b


test2 :: Int -> String -> Maybe Int
test2 a b = case readMaybe b of
    Nothing -> Nothing
    Just x -> Just $ a + x 

test3 :: (Show b) => Int -> b -> String
test3 a b = show a ++ " " ++ show b

$(generateLoggingFunctions ['test1])
