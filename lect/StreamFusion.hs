-- data DelayedList a = forall s. DelayedList (s -> Maybe (a, s)) s
sqr :: Int -> Int
sqr x = x * x


data Stream a = forall s. Stream (s -> Step a s) s

data Step a s
  = Yield a s
  | Skip s
  | Done

stream :: [a] -> Stream a -- aka fromLinkedList
stream = Stream f
  where
    f []     = Done
    f (x:xs) = Yield x xs

unstream :: Stream a -> [a] -- aka toLinkedList
unstream (Stream f s) = go s
  where
    go s' = case f s' of
      Done        -> []
      Yield x s'' -> x : go s''

{-# RULES
"stream/unstream" forall (s :: Stream a). stream (unstream s) = s
  #-}

map3 :: (a -> b) -> [a] -> [b]
map3 f = unstream . map3' f . stream

map3' :: (a -> b) -> Stream a -> Stream b
map3' g (Stream f s) = Stream h s
  where
    h s' = case f s' of
      Done        -> Done
      Yield x s'' -> Yield (g x) s''

foldr3 :: (a -> b -> b) -> b -> [a] -> b
foldr3 f z = foldr3' f z . stream

foldr3' :: (a -> b -> b) -> b -> Stream a -> b
foldr3' g b (Stream f s) = go b s
  where
    go b' s' = case f s' of
      Done        -> b'
      Yield x s'' -> go (g x b') s''

fused3 :: [Int] -> Int
fused3 = foldr3 (+) 0 . map3 sqr
--       foldr3' (+) 0 . stream . unstream . map3' sqr . stream
--                       ^               ^
--                       | nuked by rule |
--                       +---------------+
--       foldr3' (+) 0 . map3' sqr . stream

-- Filter?
filter3 :: (a -> Bool) -> [a] -> [a]
filter3 f = unstream . filter3' f . stream

filter3' :: (a -> Bool) -> Stream a -> Stream a
filter3' p (Stream f s) = Stream g s
  where
    g s' = case f s' of
      Done -> Done
      Yield x s'' ->
        if p x
          then Yield x s''
          else Skip s''

fusedFilter :: [Int] -> Int
fusedFilter = foldr3 (+) 0 . filter3 even . map3 sqr

-- benchmarking fusedFilter
-- time                 10.79 ms   (10.76 ms .. 10.82 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 10.81 ms   (10.79 ms .. 10.84 ms)
-- std dev              69.54 μs   (47.87 μs .. 118.8 μs)

-- Case                    Bytes  GCs  Check
-- fusedFilter       100,000,056  192  OK

-- If we need to skip a value, the only thing we can do is to recursively call g, which is not good, 
-- as the compiler can’t flatten, inline, and further optimize recursive functions.

-- benchmarking fusedFilter after adding Skip
-- time                 8.904 ms   (8.880 ms .. 8.926 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 8.923 ms   (8.906 ms .. 8.941 ms)
-- std dev              50.94 μs   (36.94 μs .. 74.59 μs)

-- Case                   Bytes  GCs  Check
-- fusedFilter       80,000,016  153  OK
