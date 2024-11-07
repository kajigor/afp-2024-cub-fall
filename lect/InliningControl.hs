module InliningControl where
import GHC.Float (integerToBinaryFloat')

inlining :: Int -> Int
inlining x =
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
{-# NOINLINE inlining #-}

-- ghc -O2 InliningControl.hs
-- ghc --show-iface InliningControl.hi

-- benchmarking inlining
-- time                 5.653 ms   (5.632 ms .. 5.673 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 5.614 ms   (5.601 ms .. 5.627 ms)
-- std dev              39.86 μs   (33.20 μs .. 48.70 μs)

-- benchmarking inlining with inlineable
-- time                 5.455 ms   (5.442 ms .. 5.471 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 5.447 ms   (5.432 ms .. 5.458 ms)
-- std dev              38.08 μs   (28.36 μs .. 58.38 μs)
