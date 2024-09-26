{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MonadComprehensions #-}
module Stream where

import Control.Applicative (Alternative (..))

-- Nondeterminism can be modelled with lists, but they perform depth-first search 
-- ghci> take 10 pairs 
-- [('a',0),('a',1),('a',2),('a',3),('a',4),('a',5),('a',6),('a',7),('a',8),('a',9)]
pairs :: [(Char, Int)]
pairs = [ (x, y) | x <- ['a' .. 'z'], y <- [0 ..] ]

-- Data type Stream is a simplified version of the interlieving search implementation 
-- More in "Backtracking, Interleaving, and Terminating Monad Transformers":
-- https://okmij.org/ftp/papers/LogicT.pdf
newtype Stream a = Stream { getStream :: [a] }
  deriving (Functor, Show)

instance Applicative Stream where
  pure = Stream . pure
  Stream [] <*> _ = Stream []
  _ <*> Stream [] = Stream []
  Stream (f:s) <*> Stream (h:t) = Stream (f h : getStream (Stream s <*> Stream t))

instance Alternative Stream where
  empty = Stream []
  Stream [] <|> y = y
  Stream (h:t) <|> y = Stream (h : getStream (y <|> Stream t))

instance Monad Stream where
  Stream [] >>= _ = Stream []
  Stream (h:t) >>= f = f h <|> (Stream t >>= f)

takeS :: Int -> Stream a -> Stream a
takeS n (Stream (h:t)) | n > 0 = Stream (h : getStream (takeS (n - 1) (Stream t)))
takeS _ _ = Stream []

-- ghci> takeS 10 pairs'
-- Stream {getStream = [('a',0),('b',0),('a',1),('c',0),('a',2),('b',1),('a',3),('d',0),('a',4),('b',2)]}
pairs' :: Stream (Char, Int)
pairs' = [ (x, y) | x <- Stream ['a' .. 'z'], y <- Stream [0 ..] ]