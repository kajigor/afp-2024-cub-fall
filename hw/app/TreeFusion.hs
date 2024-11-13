module TreeFusion where

sqr :: Int -> Int
sqr x = x * x

data BinaryTree a = BinaryLeaf | BinaryNode a (BinaryTree a) (BinaryTree a)
  deriving (Show, Eq)

-- Task 1
buildBinaryTree :: (forall b. (a -> b -> b -> b) -> b -> b) -> BinaryTree a
buildBinaryTree g = g BinaryNode BinaryLeaf
{-# INLINE [0] buildBinaryTree #-}

mapBinaryTreeFoldrBuild :: (a -> b) -> BinaryTree a -> BinaryTree b
mapBinaryTreeFoldrBuild f tree = buildBinaryTree (\c n -> foldrBinaryTreeFoldrBuild (mapFB c f) n tree)

mapFB :: (b -> l -> l -> l) -> (a -> b) -> a -> l -> l -> l
mapFB c f x = c (f x)

filterBinaryTreeFoldrBuild :: (a -> Bool) -> BinaryTree a -> BinaryTree a
filterBinaryTreeFoldrBuild p tree = buildBinaryTree (\c n -> foldrBinaryTreeFoldrBuild (mapFB (\x l r -> if p x then c x l r else n) id) n tree)

foldrBinaryTreeFoldrBuild :: (a -> b -> b -> b) -> b -> BinaryTree a -> b
foldrBinaryTreeFoldrBuild _ b BinaryLeaf = b
foldrBinaryTreeFoldrBuild f b (BinaryNode x l r) = f x (foldrBinaryTreeFoldrBuild f b l) (foldrBinaryTreeFoldrBuild f b r)
{-# INLINE [0] foldrBinaryTreeFoldrBuild #-}

{-# RULES
"build/foldr2" forall f z (g :: forall b. (a -> b -> b -> b) -> b -> b). foldrBinaryTreeFoldrBuild f z (buildBinaryTree g) = g f z
  #-}

fusedBinaryTreeFoldrBuildTest :: BinaryTree Int -> Int
fusedBinaryTreeFoldrBuildTest tree = foldrBinaryTreeFoldrBuild (\x l r -> (abs l + 1) * (abs r + 1) * x) 1
    (filterBinaryTreeFoldrBuild (\x -> x `rem` 400 /= 0)
      (mapBinaryTreeFoldrBuild (+1)
        (mapBinaryTreeFoldrBuild sqr tree)))

-- Task 2
data Stream a = forall s. Stream (s -> Step a s) s

data Step a s
  = Yield a s s
  | Skip s
  | Done

stream :: BinaryTree a -> Stream a
stream = Stream f
  where
    f BinaryLeaf     = Done
    f (BinaryNode x l r) = Yield x l r
{-# NOINLINE stream #-}

unstream :: Stream a -> BinaryTree a
unstream (Stream f s) = go s
  where
    go s' = case f s' of
      Done        -> BinaryLeaf
      Yield x l r -> BinaryNode x (go l) (go r)
{-# NOINLINE unstream #-}

{-# RULES
"stream/unstream" forall (s :: Stream a). stream (unstream s) = s
  #-}

map3 :: (a -> b) -> BinaryTree a -> BinaryTree b
map3 f = unstream . map3' f . stream

map3' :: (a -> b) -> Stream a -> Stream b
map3' g (Stream f s) = Stream h s
  where
    h s' = case f s' of
      Done        -> Done
      Yield x l r -> Yield (g x) l r

foldr3' :: (a -> b -> b -> b) -> b -> Stream a -> b
foldr3' g b (Stream f s) = go b s
  where
    go b' s' = case f s' of
      Done        -> b'
      Yield x l r -> g x (go b' l) (go b' r)

filter3' :: (a -> Bool) -> Stream a -> Stream a
filter3' p (Stream f s) = Stream g s
  where
    g s' = case f s' of
      Done -> Done
      Yield x l r ->
        if p x
          then Yield x l r
          else Skip l


mapBinaryTreeStream :: (a -> b) -> BinaryTree a -> BinaryTree b
mapBinaryTreeStream f = unstream . map3' f . stream

filterBinaryTreeStream :: (a -> Bool) -> BinaryTree a -> BinaryTree a
filterBinaryTreeStream p = unstream . filter3' p . stream

foldrBinaryTreeStream :: (a -> b -> b -> b) -> b -> BinaryTree a -> b
foldrBinaryTreeStream f z = foldr3' f z . stream

fusedBinaryTreeStreamTest :: BinaryTree Int -> Int
fusedBinaryTreeStreamTest tree = foldrBinaryTreeStream (\x l r -> (abs l + 1) * (abs r + 1) * x) 1
    (filterBinaryTreeStream (\x -> x `rem` 400 /= 0)
      (mapBinaryTreeStream (+1)
        (mapBinaryTreeStream sqr tree)))

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

