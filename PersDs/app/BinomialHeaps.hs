module BinomialHeaps where

-- Node rank el 
data BinomialTree a = Node Int a ([BinomialTree a]) deriving Show

type BinomialHeap a = [BinomialTree a]

link :: Ord a => BinomialTree a -> BinomialTree a -> BinomialTree a
link t1@(Node rank1 x1 c1) t2@(Node rank2 x2 c2)
    | x1 <= x2  = Node (rank1 + 1) x1 (t2 : c1)
    | otherwise = Node (rank1 + 1) x2 (t1 : c2)

-- let tree1 = Node 0 6 []
-- let tree2 = Node 0 1 []
-- let tree3 = Node 1 7 [Node 0 12 []]
-- let tree4 = Node 1 1 [Node 0 3 []]
-- insertTree tree1 [tree2, tree3]
-- let tree5 = [ Node 0 6 [], Node 1 6 [Node 0 6 []], Node 2 1 [Node 1 7 [Node 0 12 []],Node 0 3 []]]

rank :: Ord a => BinomialTree a -> Int
rank (Node r _ _) = r

root :: Ord a => BinomialTree a -> a
root (Node _ el _) = el

-- insert tree in binomial heap 0(log n)
insertTree :: Ord a => BinomialTree a -> BinomialHeap a -> BinomialHeap a
insertTree t [] = [t]
insertTree t h@(t':ts)
  | rank t < rank t' = t : h
  | otherwise        = insertTree (link t t') ts

insert :: Ord a => a -> BinomialHeap a -> BinomialHeap a
insert x heap = insertTree (Node 0 x []) heap

merge :: Ord a => BinomialHeap a -> BinomialHeap a -> BinomialHeap a
merge [] h = h
merge h [] = h
merge h1@(t1:t1s) h2@(t2:t2s)
  | rank t1 < rank t2 = t1 : merge t1s h2
  | rank t2 < rank t1 = t2 : merge h1 t2s
  | otherwise         = insertTree (link t1 t2) (merge t1s t2s)


removeMinTree :: Ord a => BinomialHeap a -> (BinomialTree a, BinomialHeap a)
removeMinTree [t] = (t, [])
removeMinTree (t:ts)
  | root t <= root t' = (t, ts)
  | otherwise         = (t', t : rest)
  where
    (t', rest) = removeMinTree ts

findMin :: Ord a => BinomialHeap a -> a
findMin ts = root t
        where (t, _) = removeMinTree ts


deleteMin :: Ord a => BinomialHeap a -> BinomialHeap a
deleteMin h = merge (reverse children) rest
  where
    (Node _ _ children, rest) = removeMinTree h

