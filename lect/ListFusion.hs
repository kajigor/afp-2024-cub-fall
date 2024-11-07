sqr :: Int -> Int
sqr x = x * x

data Array a = Array Int (Int -> a)

rangeArr :: Int -> Array Int
rangeArr n = Array n id

mapArr :: (a -> b) -> Array a -> Array b
mapArr f (Array size g) = Array size (f . g)

foldrArr :: (a -> b -> b) -> b -> Array a -> b
foldrArr f b (Array size g) = go 0 b
  where
    go n b' | n < size  = go (n + 1) (f (g n) b')
            | otherwise = b'

fusedArr :: Int -> Int
fusedArr = foldrArr (+) 0 . mapArr sqr . rangeArr

-- So called delayed arrays
-- Array is not a list, it is a function that can produce elements on demand
-- All we do is changing functions, that's why it is so efficient

-- Let's apply same idea to lists
-- But what function is the equivalent of Array?

uncons :: [a] -> Maybe (a, [a])
uncons []     = Nothing
uncons (a:as) = Just (a, as)

data DelayedList a = forall s. DelayedList (s -> Maybe (a, s)) s
map1 :: (a -> b) -> DelayedList a -> DelayedList b
map1 g (DelayedList f s) = DelayedList h s
    where
        h as = case f as of
            Nothing      -> Nothing
            Just (a, as') -> Just (g a, as')
foldr1 :: (a -> b -> b) -> b -> DelayedList a -> b
foldr1 g b (DelayedList f s) = go b s
    where
        go b' as = case f as of
            Nothing      -> b'
            Just (a, as') -> g a (go b' as')

fromLinkedList :: [a] -> DelayedList a
fromLinkedList = DelayedList uncons

toLinkedList :: DelayedList b -> [b]
toLinkedList (DelayedList f s) = unfoldr f s

unfoldr :: (s -> Maybe (a, s)) -> s -> [a]
unfoldr f s = case f s of
  Nothing      -> []
  Just (x, s') -> x : unfoldr f s'

fused1 :: [Int] -> Int
fused1 = foldr1 (+) 0 . map1 sqr . fromLinkedList

-- benchmarking fused1
-- time                 3.422 ms   (3.412 ms .. 3.433 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 3.432 ms   (3.427 ms .. 3.440 ms)
-- std dev              19.74 μs   (14.15 μs .. 29.65 μs)

-- Case                 Bytes  GCs  Check
-- fused1          80,000,016  153  OK