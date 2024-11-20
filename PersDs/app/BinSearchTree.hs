module BinSearchTree where

data BinSearchTree a = Leaf | Node (BinSearchTree a) a (BinSearchTree a)
  deriving Show

instance (Eq a) => Eq (BinSearchTree a) where
  Leaf == Leaf = True
  Node l1 v1 r1 == Node l2 v2 r2 = l1 == l2 && v1 == v2 && r1 == r2
  _ == _ = False

isMember :: Ord a =>  BinSearchTree a -> a -> Bool
isMember = undefined

-- insert element to binary search tree
insert :: Ord a => BinSearchTree a -> a -> (BinSearchTree a)
insert = undefined

-- find minimun in binary search tree
findMin :: Ord a => BinSearchTree a -> a
findMin = undefined

-- delete element from binary search tree
delete :: (Ord a) => BinSearchTree a -> a -> BinSearchTree a
delete = undefined

