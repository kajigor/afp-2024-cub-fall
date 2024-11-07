-- Fusion is a technique that allows us to avoid constructing intermediate results when chaining operations.

map0 :: (a -> b) -> [a] -> [b]
map0 _ []     = []
map0 f (x:xs) = f x : map0 f xs

foldr0 :: (a -> b -> b) -> b -> [a] -> b
foldr0 _ b []     = b
foldr0 f b (a:as) = foldr0 f (f a b) as

nofusion0 :: [Int] -> Int
nofusion0 = foldr0 (+) 0 . map0 sqr

sqr :: Int -> Int
sqr x = x * x

-- nofusion0 [0..1000000] performs:

-- benchmarking nofusion0
-- time                 155.4 ms   (146.4 ms .. 162.4 ms)
--                      0.996 R²   (0.980 R² .. 1.000 R²)
-- mean                 155.1 ms   (151.3 ms .. 159.0 ms)
-- std dev              5.522 ms   (3.154 ms .. 7.537 ms)

-- Case                  Bytes  GCs  Check
-- nofusion0       249,259,656  448  OK

manuallyFused :: [Int] -> Int
manuallyFused []     = 0
manuallyFused (x:xs) = x * x + manuallyFused xs

-- benchmarking manuallyFused
-- time                 17.10 ms   (16.71 ms .. 17.54 ms)
--                      0.996 R²   (0.992 R² .. 0.998 R²)
-- mean                 17.18 ms   (16.87 ms .. 17.62 ms)
-- std dev              932.8 μs   (673.7 μs .. 1.453 ms)

-- Case                 Bytes  GCs  Check
-- manuallyFused   96,646,160  153  OK