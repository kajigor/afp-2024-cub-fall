module TreeFusion where

sqr :: Int -> Int
sqr x = x * x

data BinaryTree a = BinaryLeaf | BinaryNode a (BinaryTree a) (BinaryTree a)
  deriving (Show, Eq)

-- Task 1

-- mapFB ::  (elt -> lst -> lst) -> (a -> elt) -> a -> lst -> lst
-- {-# INLINE [0] mapFB #-} -- See Note [Inline FB functions] in GHC.Internal.List
-- mapFB c f = \x ys -> c (f x) ys
-- mapFB c f = \x ys -> c ((f . g) x) ys

-- map2 f (map2 g xs)
-- build (\c n -> foldr2 (mapFB c f) n (map2 g xs))
-- build (\c n -> foldr2 (mapFB c f) n (build (\c n -> foldr2 (mapFB c g) n xs)))
-- build (\c n -> (\c n -> (foldr2 (mapFB c g) n xs) (mapFB c f) n)

-- build (\c n -> (foldr2 (mapFB (mapFB c f) g) n xs))
-- build (\c n -> (foldr2 (mapFB c (f . g)) n xs)) |
-- build (\c n -> (foldr2 (mapFB (\x ys -> c (f x) ys) g) n xs))
-- build (\c n -> (foldr2 (\x ys -> (\x ys -> c (f x) ys) (g x) ys) n xs))
-- build (\c n -> (foldr2 (\x ys -> c (f (g x)) ys) n xs))
-- build (\c n -> (foldr2 (c (f (g n)) xs)))
-- build (\c n -> (foldr2 (c (f (g n)) xs) n xs))

-- build (\c n -> (foldr2 (mapFB c (f . g)) n xs)) 
-- build (\c n -> (foldr2 (\x ys -> c ((f . g) x) ys) n xs)) 
-- build (\c n -> (foldr2 (c ((f . g) n) xs) n xs)) 

{-# RULES
"build/foldr2" forall f z (g :: forall b. (a -> b -> b -> b) -> b -> b). foldrBinaryTreeFoldrBuild f z (buildBinaryTree g) = g f z
"filterFB"        forall c p q. filterFB (filterFB c p) q = filterFB c (\x -> q x && p x)
"mapFB"     forall c f g.       mapFB (mapFB c f) g     = mapFB c (f.g)
  #-}

{-# INLINE [1] buildBinaryTree #-}
buildBinaryTree :: (forall b. (a -> b -> b -> b) -> b -> b) -> BinaryTree a
buildBinaryTree g = g BinaryNode BinaryLeaf

{-# INLINE [0] mapFB #-}
mapFB :: (b -> l -> l -> l) -> (a -> b) -> a -> l -> l -> l
mapFB c f = \x l r -> c (f x) l r

mapBinaryTreeFoldrBuild :: (a -> b) -> BinaryTree a -> BinaryTree b
mapBinaryTreeFoldrBuild f xs = 
  buildBinaryTree (\c n -> foldrBinaryTreeFoldrBuild (mapFB c f) n xs)

filterBinaryTreeFoldrBuild :: (a -> Bool) -> BinaryTree a -> BinaryTree a
filterBinaryTreeFoldrBuild f xs =  buildBinaryTree (\c n -> foldrBinaryTreeFoldrBuild (filterFB c f) n xs)

{-# INLINE [0] filterFB #-}
filterFB :: (a -> b -> b -> b) -> (a -> Bool) -> a -> b -> b -> b
filterFB c p x r l | p x       = c x r l
                   | otherwise = r

{-# INLINE [0] foldrBinaryTreeFoldrBuild #-}
foldrBinaryTreeFoldrBuild :: (a -> b -> b -> b) -> b -> BinaryTree a -> b
foldrBinaryTreeFoldrBuild _ z BinaryLeaf = z
foldrBinaryTreeFoldrBuild f z (BinaryNode val l r) = f val (foldrBinaryTreeFoldrBuild f z l) (foldrBinaryTreeFoldrBuild f z r)

fusedBinaryTreeFoldrBuildTest :: BinaryTree Int -> Int
fusedBinaryTreeFoldrBuildTest tree = foldrBinaryTreeFoldrBuild (\x l r -> (abs l + 1) * (abs r + 1) * x) 1 ((mapBinaryTreeFoldrBuild (+1) . filterBinaryTreeFoldrBuild (\x -> x `rem` 400 /= 0) . mapBinaryTreeFoldrBuild sqr) tree)

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

