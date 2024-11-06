{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module E1_LoggingExercise where

import Language.Haskell.TH
import E2_LoggingExercise
import Control.Monad (replicateM)

test1 :: Int -> Int -> String
test1 a b = show a ++ " " ++ show b

test2 :: (Show b) => Int -> b -> String
test2 a b = show a ++ " " ++ show b

$(generateLoggingFunction 'test1)
