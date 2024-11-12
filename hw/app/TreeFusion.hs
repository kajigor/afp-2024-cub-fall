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
mapBinaryTreeFoldrBuild g t =
  buildBinaryTree $ \n z -> foldrBinaryTreeFoldrBuild (n . g) z t

filterBinaryTreeFoldrBuild :: (a -> Bool) -> BinaryTree a -> BinaryTree a
filterBinaryTreeFoldrBuild p t =
  buildBinaryTree $ \n z -> foldrBinaryTreeFoldrBuild (\x l r -> if p x then n x l r else z) z t

{-
I kinda don't understand: do I have to implement foldr or fold here?
If I keep the same signature as in `foldrBinaryTreeSimple`
it is hard to use it in `map` and `filter`.
-}
foldrBinaryTreeFoldrBuild :: (a -> b -> b -> b) -> b -> BinaryTree a -> b
foldrBinaryTreeFoldrBuild _ z BinaryLeaf = z
foldrBinaryTreeFoldrBuild g z (BinaryNode v l r) =
  g v (foldrBinaryTreeFoldrBuild g z l) (foldrBinaryTreeFoldrBuild g z r)
{-# INLINE [0] foldrBinaryTreeFoldrBuild #-}

{-
But with change of signature of `foldrBinaryTreeFoldrBuild`
it is hard to translate the last fold function in this test.
-}
fusedBinaryTreeFoldrBuildTest :: BinaryTree Int -> Int
fusedBinaryTreeFoldrBuildTest tree = foldrBinaryTreeFoldrBuild (\x l r -> (abs ((abs r + 1) * l) + 1) * x) 1 ((mapBinaryTreeFoldrBuild (+ 1) . filterBinaryTreeFoldrBuild (\x -> x `rem` 400 /= 0) . mapBinaryTreeFoldrBuild sqr) tree)

{-# RULES
"tree build/foldr" forall f z (g :: forall b. (a -> b -> b -> b) -> b -> b). foldrBinaryTreeFoldrBuild f z (buildBinaryTree g) = g f z
  #-}

-- Task 2

data BStream a = forall s. BStream (s -> BStep a s) s

data BStep a s = Branch a s s | Stop

streamMap :: (a -> b) -> BStream a -> BStream b
streamMap f (BStream g s) = BStream h s
  where
    h s' = case g s' of
      Stop -> Stop
      Branch x l r -> Branch (f x) l r

streamFilter :: (a -> Bool) -> BStream a -> BStream a
streamFilter p (BStream g s) = BStream h s
  where
    h s' = case g s' of
      Stop -> Stop
      Branch x l r -> if p x then Branch x l r else Stop

streamFold :: (a -> b -> b -> b) -> b -> BStream a -> b
streamFold f z (BStream g s) = go z s
  where
    go a s' = case g s' of
      Stop -> a
      Branch x l r -> f x (go a l) (go a r)

streamify :: BinaryTree a -> BStream a
streamify = BStream g
  where
    g BinaryLeaf = Stop
    g (BinaryNode v l r) = Branch v l r
{-# INLINE [0] streamify #-}

unstreamify :: BStream a -> BinaryTree a
unstreamify (BStream g s) = case g s of
  Stop -> BinaryLeaf
  Branch v l r -> BinaryNode v (unstreamify (BStream g l)) (unstreamify (BStream g r))
{-# INLINE [0] unstreamify #-}

{-# RULES
"streamify/unstreamify" forall s. streamify (unstreamify s) = s
  #-}

mapBinaryTreeStream :: (a -> b) -> BinaryTree a -> BinaryTree b
mapBinaryTreeStream f = unstreamify . streamMap f . streamify

filterBinaryTreeStream :: (a -> Bool) -> BinaryTree a -> BinaryTree a
filterBinaryTreeStream p = unstreamify . streamFilter p . streamify

foldrBinaryTreeStream :: (a -> b -> b -> b) -> b -> BinaryTree a -> b
foldrBinaryTreeStream f z = streamFold f z . streamify

fusedBinaryTreeStreamTest :: BinaryTree Int -> Int
fusedBinaryTreeStreamTest tree = foldrBinaryTreeStream (\x l r -> (abs ((abs r + 1) * l) + 1) * x) 1 ((mapBinaryTreeStream (+ 1) . filterBinaryTreeStream (\x -> x `rem` 400 /= 0) . mapBinaryTreeStream sqr) tree)

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
binaryTreeTest tree = foldrBinaryTreeSimple (\x y -> (abs x + 1) * y) 1 ((mapBinaryTreeSimple (+ 1) . filterBinaryTreeSimple (\x -> x `rem` 400 /= 0) . mapBinaryTreeSimple sqr) tree)
