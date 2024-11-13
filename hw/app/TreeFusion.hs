module TreeFusion where

sqr :: Int -> Int
sqr x = x * x

data BinaryTree a = BinaryLeaf | BinaryNode a (BinaryTree a) (BinaryTree a)
  deriving (Show, Eq)

-- Task 1

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

{-# RULES
"stream/unstream" forall (s :: Stream a). stream (unstream s) = s
  #-}

data Stream a = forall s. Stream (s -> Step a s) s

data Step a s
  = Yield a s s
  | Done

{-# NOINLINE stream #-}
stream :: BinaryTree a -> Stream a
stream = Stream f
  where
    f BinaryLeaf     = Done
    f (BinaryNode x l r) = Yield x l r

{-# NOINLINE unstream #-}
unstream :: Stream a -> BinaryTree a
unstream (Stream f s) = go s
  where
    go s' = case f s' of
      Done        -> BinaryLeaf
      Yield x l r -> BinaryNode x (go l) (go r)

mapBinaryTreeStream :: (a -> b) -> BinaryTree a -> BinaryTree b
mapBinaryTreeStream f = unstream . mapBinaryTreeStream' f . stream

mapBinaryTreeStream' :: (a -> b) -> Stream a -> Stream b
mapBinaryTreeStream' g (Stream f s) = Stream h s
  where
    h s' = case f s' of
      Done        -> Done
      Yield x l r -> Yield (g x) l r

filterBinaryTreeStream :: (a -> Bool) -> BinaryTree a -> BinaryTree a
filterBinaryTreeStream f = unstream . filterBinaryTreeStream' f . stream

filterBinaryTreeStream' :: (a -> Bool) -> Stream a -> Stream a
filterBinaryTreeStream' p (Stream f s) = Stream g s
  where
    g s' = case f s' of
      Done -> Done
      Yield x l r ->
        if p x
          then Yield x l r
          else Done

foldrBinaryTreeStream :: (a -> b -> b -> b) -> b -> BinaryTree a -> b
foldrBinaryTreeStream f z = foldrBinaryTreeStream' f z . stream

foldrBinaryTreeStream' :: (a -> b -> b -> b) -> b -> Stream a -> b
foldrBinaryTreeStream' g b (Stream f s) = go b s
  where
    go b' s' = case f s' of
      Done        -> b'
      Yield x l r -> g x (go b' l) (go b' r)

fusedBinaryTreeStreamTest :: BinaryTree Int -> Int
fusedBinaryTreeStreamTest tree = foldrBinaryTreeStream (\x l r -> (abs l + 1) * (abs r + 1) * x) 1 ((mapBinaryTreeStream (+1) . filterBinaryTreeStream (\x -> x `rem` 400 /= 0) . mapBinaryTreeStream sqr) tree)

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

