module Specializing 
    (special0')
where

special0' :: (Num a, Enum a) => a -> a
special0' x =
  product [x..1000000] +
  product [x..1000001] +
  product [x..1000002] +
  product [x..1000003] +
  product [x..1000004] +
  product [x..1000005] +
  product [x..1000006] +
  product [x..1000007] +
  product [x..1000008] +
  product [x..1000009] +
  product [x..1000010]
{-# SPECIALIZE special0' :: Int -> Int #-}

special0 :: Int -> Int
special0 x = special0' x `rem` 10

-- ghc -O2 Specializing.hs
-- ghc --show-iface Specializing.hi

-- benchmarking special0
-- time                 5.457 ms   (5.436 ms .. 5.477 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 5.481 ms   (5.470 ms .. 5.492 ms)
-- std dev              35.69 μs   (29.94 μs .. 44.88 μs)

-- benchmarking special0_alt   <---- defined in a separate module
-- time                 5.462 ms   (5.436 ms .. 5.496 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 5.472 ms   (5.458 ms .. 5.485 ms)
-- std dev              41.42 μs   (33.29 μs .. 55.02 μs)

-- benchmarking special0_alt_after_removing_export_of_special0
-- time                 912.0 ms   (866.2 ms .. 947.7 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 931.0 ms   (919.8 ms .. 939.9 ms)
-- std dev              13.88 ms   (0.0 s .. 15.45 ms)

-- benchmarking special0_alt_after_removing_export_of_special0_and_adding_specialization
-- time                 5.392 ms   (5.381 ms .. 5.403 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 5.399 ms   (5.392 ms .. 5.408 ms)
-- std dev              25.12 μs   (16.60 μs .. 38.90 μs)