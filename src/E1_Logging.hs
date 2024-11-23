{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
module E1_Logging where

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

test3 :: Show a => a -> [a]
test3 = replicate 6 

$(generateLoggingFunctions ['test1, 'test2, 'test3])

logs :: String
logs = fst $ do
    _ <- test1Logged 1 2
    _ <- test2Logged 1 "3"
    _ <- test3Logged "element"
    return ()
