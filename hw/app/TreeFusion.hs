module TreeFusion where

sqr :: Int -> Int
sqr x = x * x

testF1 :: Num a => a -> a -> a
testF1 x y = (abs x + 1) * y
testF2 :: Integral a => a -> Bool
testF2 x = x `rem` 400 /= 0

data BinaryTree a = BinaryLeaf | BinaryNode a (BinaryTree a) (BinaryTree a)
  deriving (Show, Eq)

-- Task 1

buildBinaryTree :: (forall b. (a -> b -> b -> b) -> b -> b) -> BinaryTree a
buildBinaryTree g = g BinaryNode BinaryLeaf

mapBinaryTreeFoldrBuild :: (a -> b) -> BinaryTree a -> BinaryTree b
mapBinaryTreeFoldrBuild = undefined

filterBinaryTreeFoldrBuild :: (a -> Bool) -> BinaryTree a -> BinaryTree a
filterBinaryTreeFoldrBuild = undefined

foldrBinaryTreeFoldrBuild :: (b -> a -> b) -> b -> BinaryTree a -> b
foldrBinaryTreeFoldrBuild = undefined

fusedBinaryTreeFoldrBuildTest :: BinaryTree Int -> Int
fusedBinaryTreeFoldrBuildTest = undefined -- see binaryTreeTest

-- Task 2

data Stream a = forall t. Stream (t -> Step a t) t

data Step a t = Yield a t t | Done

stream :: BinaryTree a -> Stream a
stream = Stream f
  where
    f (BinaryNode v l r) = Yield v l r
    f BinaryLeaf = Done
{-#NOINLINE stream #-}

unstream :: Stream a -> BinaryTree a
unstream (Stream f t) = go t
  where
    go t' = case f t' of
      Yield v l r -> BinaryNode v (go l) (go r)
      Done -> BinaryLeaf
{-#NOINLINE unstream #-}
      

{-# RULES
  "binary tree stream/unstream" forall (s :: Stream a). stream (unstream s) = s
#-}

mapBinaryTreeStream :: (a -> b) -> BinaryTree a -> BinaryTree b
mapBinaryTreeStream map_f = unstream . aux . stream
  where
    aux (Stream f t') = Stream h t'
      where
        h t = case f t of
          Yield v l r -> Yield (map_f v) l r
          Done -> Done

filterBinaryTreeStream :: (a -> Bool) -> BinaryTree a -> BinaryTree a
filterBinaryTreeStream p = unstream . aux . stream
  where
    aux (Stream f t') = Stream h t'
      where
        h t = case f t of
          n@(Yield v _ _) -> if p v then n else Done
          Done -> Done

foldrBinaryTreeStream :: (b -> a -> b) -> b -> BinaryTree a -> b
foldrBinaryTreeStream func b = aux . stream
  where
    aux (Stream f t') = go b t'
      where
        go ini t = case f t of
          Yield v l r -> go (go ini r) l `func` v
          Done -> ini

fusedBinaryTreeStreamTest :: BinaryTree Int -> Int
fusedBinaryTreeStreamTest = foldrBinaryTreeStream testF1 1 . mapBinaryTreeStream (+1) . filterBinaryTreeStream testF2 . mapBinaryTreeStream sqr

-- Naive implementations for comparing performance and logic

mapBinaryTreeSimple :: (a -> b) -> BinaryTree a -> BinaryTree b
mapBinaryTreeSimple _ BinaryLeaf = BinaryLeaf
mapBinaryTreeSimple f (BinaryNode val l r) = BinaryNode (f val) (mapBinaryTreeSimple f l) (mapBinaryTreeSimple f r)

filterBinaryTreeSimple :: (a -> Bool) -> BinaryTree a -> BinaryTree a
filterBinaryTreeSimple _ BinaryLeaf = BinaryLeaf
filterBinaryTreeSimple f (BinaryNode val l r) = if f val then BinaryNode val (filterBinaryTreeSimple f l) (filterBinaryTreeSimple f r) else BinaryLeaf

foldrBinaryTreeSimple :: (b -> a -> b) -> b -> BinaryTree a -> b
foldrBinaryTreeSimple _ z BinaryLeaf = z
foldrBinaryTreeSimple f z (BinaryNode val l r) = f (foldrBinaryTreeSimple f (foldrBinaryTreeSimple f z r) l) val

binaryTreeTest :: BinaryTree Int -> Int
binaryTreeTest tree = foldrBinaryTreeSimple testF1 1 ((mapBinaryTreeSimple (+1) . filterBinaryTreeSimple testF2 . mapBinaryTreeSimple sqr) tree)

