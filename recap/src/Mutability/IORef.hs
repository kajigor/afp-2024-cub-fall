{-# LANGUAGE TypeApplications #-}

module Mutability.IORef where

import Control.Monad (when)
import Data.Foldable (forM_)
import Data.IORef

main = do
  n <- read <$> getLine
  x <- newIORef n
  increment x
  increment x
  counter <- readIORef x
  putStrLn "The counter is"
  print counter

increment :: IORef Int -> IO ()
increment ref =
  modifyIORef ref (+ 1)

collectUserInputs :: IO ()
collectUserInputs = do
  inputsRef <- newIORef []
  collectInputs inputsRef
  inputs <- readIORef inputsRef
  putStrLn "You entered:"
  mapM_ putStrLn (reverse inputs)

collectInputs :: IORef [String] -> IO ()
collectInputs ref = do
  line <- getLine
  if line == "exit"
    then return ()
    else do
      modifyIORef ref (line :)
      collectInputs ref

bubbleSort :: [Int] -> IO [Int]
bubbleSort input = do
  let ln = length input
  xs <- mapM newIORef input

  forM_ [0 .. ln - 1] $ \_ -> do
    forM_ [0 .. ln - 2] $ \j -> do
      let ix = xs !! j
      let iy = xs !! (j + 1)

      x <- readIORef ix
      y <- readIORef iy

      when (x > y) $ do
        writeIORef ix y
        writeIORef iy x

  mapM readIORef xs
