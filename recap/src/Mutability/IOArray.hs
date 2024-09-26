module Mutability.IOArray where

import Control.Monad (when)
import Data.Array.IO (IOArray, newArray, readArray, writeArray, getElems, getBounds, newListArray)
import Data.Foldable (forM_)

foo :: IO ()
foo = do
  arr <- newArray (1, 10) 13 :: IO (IOArray Int Int)
  a <- readArray arr 1
  writeArray arr 1 42
  b <- readArray arr 1
  print (a, b)

bubbleSort :: IOArray Int Int -> IO () 
bubbleSort arr = do 
  (l, r) <- getBounds arr 
  forM_ [0 .. r - l] $ \_ -> do
    forM_ [0 .. r - l - 1] $ \j -> do
      x <- readArray arr j
      y <- readArray arr (j + 1)

      when (x > y) $ do
        writeArray arr j y
        writeArray arr (j + 1) x

runBubbleSort :: [Int] -> IO [Int]
runBubbleSort input = do
  arr <- newListArray (0, length input - 1) input
  bubbleSort arr 
  getElems arr 