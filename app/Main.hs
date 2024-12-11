{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad (forM_, when)
import Control.Monad.Free (Free (..), liftF)
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.Array.IO.Safe
  ( IOArray,
    MArray (newArray),
    readArray,
    writeArray,
  )
import MatrixIO (readCsvIntoArray, writeArrayToCsv)

data MatrixF t a
  = Read String (t -> a)
  | Write String t a
  | Sum t t (t -> a)
  | AddInto t t a
  | Multiply t t (t -> a)
  deriving (Functor)

type FreeMatrix t = Free (MatrixF t)

matrixRead :: String -> FreeMatrix t t
matrixRead filename = liftF (Read filename id)

matrixWrite :: String -> t -> FreeMatrix t ()
matrixWrite filename t = liftF (Write filename t ())

matrixSum :: t -> t -> FreeMatrix t t
matrixSum t1 t2 = liftF (Sum t1 t2 id)

matrixAddInto :: t -> t -> FreeMatrix t ()
matrixAddInto t1 t2 = liftF (AddInto t1 t2 ())

matrixMultiply :: t -> t -> FreeMatrix t t
matrixMultiply t1 t2 = liftF (Multiply t1 t2 id)

data MutableMatrix = MutableMatrix Int Int (IOArray (Int, Int) Int)

interpret :: FreeMatrix MutableMatrix a -> MaybeT IO a
interpret (Pure a) = return a
interpret (Free (Read filename f)) = do
  (matrix, x, y) <- readCsvIntoArray filename
  interpret (f (MutableMatrix x y matrix))
interpret (Free (Write filename t f)) = do
  let MutableMatrix _ _ m = t
  lift $ writeArrayToCsv filename m
  interpret f
interpret (Free (Sum t1 t2 f)) = do
  let MutableMatrix x1 y1 m1 = t1
  let MutableMatrix x2 y2 m2 = t2
  when (x1 /= x2 || y1 /= y2) (fail "Matrix dimensions do not match")
  m3 <- lift $ newArray ((0, 0), (x1 - 1, y1 - 1)) 0
  forM_ [0 .. x1 - 1] $ \r ->
    forM_ [0 .. y1 - 1] $ \c -> do
      a <- lift $ readArray m1 (r, c)
      b <- lift $ readArray m2 (r, c)
      lift $ writeArray m3 (r, c) (a + b)
  interpret (f (MutableMatrix x1 y1 m3))
interpret (Free (AddInto t1 t2 f)) = do
  let MutableMatrix x1 y1 m1 = t1
  let MutableMatrix x2 y2 m2 = t2
  when (x1 /= x2 || y1 /= y2) (fail "Matrix dimensions do not match")
  forM_ [0 .. x1 - 1] $ \r ->
    forM_ [0 .. y1 - 1] $ \c -> do
      a <- lift $ readArray m1 (r, c)
      b <- lift $ readArray m2 (r, c)
      lift $ writeArray m1 (r, c) (a + b)
  interpret f
interpret (Free (Multiply t1 t2 f)) = do
  let MutableMatrix x1 y1 m1 = t1
  let MutableMatrix x2 y2 m2 = t2
  when (y1 /= x2) (fail "Matrix dimensions do not match")
  m3 <- lift $ newArray ((0, 0), (x1 - 1, y2 - 1)) 0
  forM_ [0 .. x1 - 1] $ \r ->
    forM_ [0 .. y2 - 1] $ \c ->
      forM_ [0 .. y1 - 1] $ \k -> do
        a <- lift $ readArray m1 (r, k)
        b <- lift $ readArray m2 (k, c)
        s <- lift $ readArray m3 (r, c)
        lift $ writeArray m3 (r, c) (s + a * b)
  interpret (f (MutableMatrix x1 y2 m3))

example :: FreeMatrix a ()
example = do
  x <- matrixRead "input1.csv"
  y <- matrixRead "input2.csv"
  z <- matrixSum x y
  matrixAddInto z z
  matrixAddInto z z
  w <- matrixMultiply x y
  matrixWrite "output.csv" z
  matrixWrite "output2.csv" w

exampleUnmatchedSizes :: FreeMatrix a ()
exampleUnmatchedSizes = do
  x <- matrixRead "input1.csv"
  y <- matrixRead "input3.csv"
  matrixAddInto x y
  matrixWrite "output.csv" x

exampleInvalidCsv :: FreeMatrix a ()
exampleInvalidCsv = do
  x <- matrixRead "input1.csv"
  y <- matrixRead "input4.csv"
  matrixAddInto x y
  matrixWrite "output.csv" x

exampleNoFile :: FreeMatrix a ()
exampleNoFile = do
  x <- matrixRead "input1.csv"
  y <- matrixRead "input5.csv"
  matrixAddInto x y
  matrixWrite "output.csv" x

main :: IO ()
main = do
  result <- runMaybeT (interpret example)
  case result of
    Just () -> putStrLn "Success!"
    Nothing -> putStrLn "Failure!"
