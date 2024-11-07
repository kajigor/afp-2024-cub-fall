import GHC.Base (build)

sqr :: Int -> Int
sqr x = x * x

-- build :: (forall b. (a -> b -> b) -> b -> b) -> [a]
-- build g = g (:) []

-- build (\cons nil -> 1 `cons` (2 `cons` (3 `cons` nil)))

-- foldr f z (build g) = g f z
-- foldr f z [1,2,3] = 1 `f` (2 `f` (3 `f` z))

map2 :: (a -> b) -> [a] -> [b]
map2 _ []     = []
map2 f (x:xs) = f x : map2 f xs
{-# NOINLINE map2 #-} -- map2 will not be inlined because it is self-recursive, but GHC can’t figure that out (yet).

{-# RULES
"map2"     [~1] forall f xs. map2 f xs               = build (\c n -> foldr2 (mapFB c f) n xs)
"map2List" [1]  forall f.    foldr2 (mapFB (:) f) [] = map2 f
"mapFB"    forall c f g.     mapFB (mapFB c f) g     = mapFB c (f . g)
  #-}

mapFB :: (b -> l -> l) -> (a -> b) -> a -> l -> l
mapFB c f = \x ys -> c (f x) ys -- we want to facilitate inlining => use lambda
{-# INLINE [0] mapFB #-}
-- We want it to be inlined, but only at the end because some rules match on it and they would be broken if it were inlined too early.

foldr2 :: (a -> b -> b) -> b -> [a] -> b
foldr2 f z = go
  where
    go []     = z
    go (y:ys) = y `f` go ys
{-# INLINE [0] foldr2 #-}

{-# RULES
"build/foldr2" forall f z (g :: forall b. (a -> b -> b) -> b -> b). foldr2 f z (build g) = g f z
  #-}

fused2 :: [Int] -> Int
fused2 = foldr2 (+) 0 . map2 sqr

-- benchmarking fused2
-- time                 17.87 ms   (17.48 ms .. 18.33 ms)
--                      0.996 R²   (0.992 R² .. 0.998 R²)
-- mean                 17.94 ms   (17.61 ms .. 18.42 ms)
-- std dev              962.6 μs   (689.0 μs .. 1.401 ms)

-- Case                  Bytes  GCs  Check
-- fused2           96,646,160  153  OK

