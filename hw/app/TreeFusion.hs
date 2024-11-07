module TreeFusion where

sqr :: Int -> Int
sqr x = x * x

data BinaryTree a = BinaryLeaf | BinaryNode a (BinaryTree a) (BinaryTree a)
  deriving (Show, Eq)

-- Task 1

buildBinaryTree :: (forall b. (a -> b -> b) -> b -> b) -> BinaryTree a
buildBinaryTree g = undefined

mapBinaryTreeFoldrBuild :: (a -> b) -> BinaryTree a -> BinaryTree b
mapBinaryTreeFoldrBuild = undefined

filterBinaryTreeFoldrBuild :: (a -> Bool) -> BinaryTree a -> BinaryTree a
filterBinaryTreeFoldrBuild = undefined

foldrBinaryTreeFoldrBuild :: (b -> a -> b) -> b -> BinaryTree a -> b
foldrBinaryTreeFoldrBuild = undefined

fusedBinaryTreeFoldrBuildTest :: BinaryTree Int -> Int
fusedBinaryTreeFoldrBuildTest = undefined -- see binaryTreeTest

-- Task 2

mapBinaryTreeStream :: (a -> b) -> BinaryTree a -> BinaryTree b
mapBinaryTreeStream = undefined

filterBinaryTreeStream :: (a -> Bool) -> BinaryTree a -> BinaryTree a
filterBinaryTreeStream = undefined

foldrBinaryTreeStream :: (b -> a -> b) -> b -> BinaryTree a -> b
foldrBinaryTreeStream = undefined

fusedBinaryTreeStreamTest :: BinaryTree Int -> Int
fusedBinaryTreeStreamTest = undefined -- see binaryTreeTest

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
binaryTreeTest tree = foldrBinaryTreeSimple (\x y -> (abs x + 1) * y) 1 ((mapBinaryTreeSimple (+1) . filterBinaryTreeSimple (\x -> x `rem` 400 /= 0) . mapBinaryTreeSimple sqr) tree)

