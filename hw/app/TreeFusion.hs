module TreeFusion where

sqr :: Int -> Int
sqr x = x * x

data BinaryTree a = BinaryLeaf | BinaryNode a (BinaryTree a) (BinaryTree a)
  deriving (Show, Eq)

-- Task 1

{-# RULES
"build/foldr2" forall f z (g :: forall b. (a -> b -> b -> b) -> b -> b). foldrBinaryTreeFoldrBuild f z (buildBinaryTree g) = g f z
  #-}

buildBinaryTree :: (forall b. (a -> b -> b -> b) -> b -> b) -> BinaryTree a
buildBinaryTree g = g BinaryNode BinaryLeaf
{-# NOINLINE buildBinaryTree #-} 

mapBinaryTreeFoldrBuild :: (a -> b) -> BinaryTree a -> BinaryTree b
mapBinaryTreeFoldrBuild f t =
  buildBinaryTree (\c n -> foldrBinaryTreeFoldrBuild (mapFB c f) n t)

mapFB :: (b -> l -> l -> l) -> (a -> b) -> a -> l -> l -> l
mapFB c f = \x l r -> c (f x) l r -- we want to facilitate inlining => use lambda

filterBinaryTreeFoldrBuild :: (a -> Bool) -> BinaryTree a -> BinaryTree a
filterBinaryTreeFoldrBuild p t =
  buildBinaryTree (\c n -> foldrBinaryTreeFoldrBuild (mapFB (\x l r -> if p x then c x l r else n) id) n t)

foldrBinaryTreeFoldrBuild :: (a -> b -> b -> b) -> b -> BinaryTree a -> b
foldrBinaryTreeFoldrBuild _ acc BinaryLeaf = acc
foldrBinaryTreeFoldrBuild f acc (BinaryNode x left right) =
  f x (foldrBinaryTreeFoldrBuild f acc left) (foldrBinaryTreeFoldrBuild f acc right)
{-# NOINLINE foldrBinaryTreeFoldrBuild #-} 

fusedBinaryTreeFoldrBuildTest :: BinaryTree Int -> Int
fusedBinaryTreeFoldrBuildTest tree = foldrBinaryTreeFoldrBuild (\y l r -> (abs l + 1) * (abs r + 1) * y) 1 ((mapBinaryTreeFoldrBuild (+1) . filterBinaryTreeFoldrBuild (\x -> x `rem` 400 /= 0) . mapBinaryTreeFoldrBuild sqr) tree)

-- Task 2

data Stream a = forall s. Stream (s -> Step a s) s

data Step a s
  = Yield a s s
  | Done

stream :: BinaryTree a -> Stream a
stream = Stream f
  where
    f BinaryLeaf         = Done
    f (BinaryNode x l r) = Yield x l r
{-# NOINLINE stream #-} 

unstream :: Stream a -> BinaryTree a
unstream (Stream f s) = go s
  where
    go s' = case f s' of
      Done          -> BinaryLeaf
      Yield x s1 s2 -> BinaryNode x (go s1) (go s2)
{-# NOINLINE unstream #-} 

{-# RULES
"stream/unstream" forall (s :: Stream a). stream (unstream s) = s
  #-}

map3' :: (a -> b) -> Stream a -> Stream b
map3' g (Stream f s) = Stream h s
  where
    h s' = case f s' of
      Done          -> Done
      Yield x s1 s2 -> Yield (g x) s1 s2

filter3' :: (a -> Bool) -> Stream a -> Stream a
filter3' g (Stream f s) = Stream h s
  where
    h s' = case f s' of
      Done -> Done
      Yield x s1 s2 ->
        if g x
          then Yield x s1 s2
          else Done

mapBinaryTreeStream :: (a -> b) -> BinaryTree a -> BinaryTree b
mapBinaryTreeStream f = unstream . map3' f . stream

filterBinaryTreeStream :: (a -> Bool) -> BinaryTree a -> BinaryTree a
filterBinaryTreeStream f = unstream . filter3' f . stream

foldr3' :: (a -> b -> b -> b) -> b -> Stream a -> b
foldr3' g b (Stream f s) = go b s
  where
    go b' s' = case f s' of
      Done        -> b'
      Yield x s1 s2 -> g x (go b' s1) (go b' s2)

foldrBinaryTreeStream :: (a -> b -> b -> b) -> b -> BinaryTree a -> b
foldrBinaryTreeStream f b = foldr3' f b . stream

fusedBinaryTreeStreamTest :: BinaryTree Int -> Int
fusedBinaryTreeStreamTest tree = foldrBinaryTreeStream (\x l r -> (abs l + 1) * (abs r + 1) * x) 1 ((mapBinaryTreeStream (+1) . filterBinaryTreeStream (\x -> x `rem` 400 /= 0) . mapBinaryTreeStream sqr) tree)

-- Naive implementations for comparing performance and logic

mapBinaryTreeSimple :: (a -> b) -> BinaryTree a -> BinaryTree b
mapBinaryTreeSimple _ BinaryLeaf = BinaryLeaf
mapBinaryTreeSimple f (BinaryNode val l r) = BinaryNode (f val) (mapBinaryTreeSimple f l) (mapBinaryTreeSimple f r)

filterBinaryTreeSimple :: (a -> Bool) -> BinaryTree a -> BinaryTree a
filterBinaryTreeSimple _ BinaryLeaf = BinaryLeaf
filterBinaryTreeSimple f (BinaryNode val l r) = if f val then BinaryNode val (filterBinaryTreeSimple f l) (filterBinaryTreeSimple f r) else BinaryLeaf

foldrBinaryTreeSimple :: (a -> b -> b -> b) -> b -> BinaryTree a -> b
foldrBinaryTreeSimple _ z BinaryLeaf = z
foldrBinaryTreeSimple f z (BinaryNode val l r) = f val (foldrBinaryTreeSimple f z l) (foldrBinaryTreeSimple f z r)

binaryTreeTest :: BinaryTree Int -> Int
binaryTreeTest tree = foldrBinaryTreeSimple (\x l r -> (abs l + 1) * (abs r + 1) * x) 1 ((mapBinaryTreeSimple (+1) . filterBinaryTreeSimple (\x -> x `rem` 400 /= 0) . mapBinaryTreeSimple sqr) tree)

