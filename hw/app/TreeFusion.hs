module TreeFusion where

sqr :: Int -> Int
sqr x = x * x

data BinaryTree a = BinaryLeaf | BinaryNode a (BinaryTree a) (BinaryTree a)
  deriving (Show, Eq)

-- Task 1

{-# RULES
"buildBinaryTree/foldrBinaryTreeFoldrBuild" forall f z (g :: forall b. (a -> b -> b -> b) -> b -> b). foldrBinaryTreeFoldrBuild f z (buildBinaryTree g) = g f z
#-}

buildBinaryTree :: (forall b. (a -> b -> b -> b) -> b -> b) -> BinaryTree a
buildBinaryTree g = g BinaryNode BinaryLeaf
{-# INLINE [1] buildBinaryTree #-}
-- changed (a -> b -> b) to (a -> b -> b -> b), because we need for build function: start element and 'wrapper' 
-- and 'wrappper' BinaryNode has type (BinaryTree a %1 -> BinaryTree a %1 -> BinaryTree a)

mapBinaryTreeFoldrBuild :: (a -> b) -> BinaryTree a -> BinaryTree b
mapBinaryTreeFoldrBuild f tree = buildBinaryTree (\c n -> 
                                      foldrBinaryTreeFoldrBuild (\a right left -> c (f a) left right) n tree)

filterBinaryTreeFoldrBuild :: (a -> Bool) -> BinaryTree a -> BinaryTree a
filterBinaryTreeFoldrBuild f tree = buildBinaryTree (\c n -> foldrBinaryTreeFoldrBuild (\a right left -> if f a then (c a right left) else n ) n tree)

foldrBinaryTreeFoldrBuild :: (a -> b -> b -> b) -> b -> BinaryTree a -> b
foldrBinaryTreeFoldrBuild _ st_el BinaryLeaf = st_el
foldrBinaryTreeFoldrBuild f st_el (BinaryNode el left right) = f el (foldrBinaryTreeFoldrBuild f st_el left) (foldrBinaryTreeFoldrBuild f st_el right)
{-# INLINE [0] foldrBinaryTreeFoldrBuild #-}
-- also changed (b -> a -> b) to (a -> b -> b -> b), because foldr :: (a->b->b) -> b -> [a] -> b

fusedBinaryTreeFoldrBuildTest :: BinaryTree Int -> Int
fusedBinaryTreeFoldrBuildTest tree = foldrBinaryTreeFoldrBuild 
                      (\x l r -> (abs x + 1) * l * r) 
                      1 
                      ((mapBinaryTreeFoldrBuild (+1) . filterBinaryTreeFoldrBuild (\x -> x `rem` 400 /= 0) . mapBinaryTreeFoldrBuild sqr) tree) -- see binaryTreeTest 

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

