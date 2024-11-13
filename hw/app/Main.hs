module Main where
import Criterion.Main
import Weigh
import TreeFusion (BinaryTree(..), binaryTreeTest, fusedBinaryTreeFoldrBuildTest, fusedBinaryTreeStreamTest)

n :: Int
n = 1000000

sampleBinaryTree :: BinaryTree Int
sampleBinaryTree = go [1..n]
    where
        go [] = BinaryLeaf
        go xs = BinaryNode (head xs) (go (take (length xs `div` 2) (drop 1 xs))) (go (drop (length xs `div` 2) (drop 1 xs)))

memoryBenchmarks :: Weigh ()
memoryBenchmarks = do
  func "not fused binary tree test" binaryTreeTest sampleBinaryTree
  func "foldr/build fused binary tree test" fusedBinaryTreeFoldrBuildTest sampleBinaryTree -- uncomment this line when you implement the foldr/build version
  func "stream fused binary tree test" fusedBinaryTreeStreamTest sampleBinaryTree -- uncomment this line when you implement the stream version

main :: IO ()
main = do
    defaultMain [
        bench "not fused binary tree test" $ whnf binaryTreeTest sampleBinaryTree
        , bench "foldr/buils fused binary tree test" $ whnf fusedBinaryTreeFoldrBuildTest sampleBinaryTree -- uncomment this line when you implement the foldr/build version
        , bench "stream fused binary tree test" $ whnf fusedBinaryTreeStreamTest sampleBinaryTree -- uncomment this line when you implement the stream version
        ]
    mainWith memoryBenchmarks


-- benchmarking not fused binary tree test
-- time                 276.8 ms   (255.2 ms .. 285.5 ms)
--                      0.998 R²   (0.994 R² .. 1.000 R²)
-- mean                 273.5 ms   (262.4 ms .. 281.1 ms)
-- std dev              11.72 ms   (7.274 ms .. 17.86 ms)
-- variance introduced by outliers: 16% (moderately inflated)

-- benchmarking foldr/buils fused binary tree test
-- time                 22.16 ms   (22.08 ms .. 22.28 ms)
--                      1.000 R²   (0.999 R² .. 1.000 R²)
-- mean                 22.19 ms   (22.13 ms .. 22.29 ms)
-- std dev              166.5 μs   (104.1 μs .. 269.1 μs)

-- benchmarking stream fused binary tree test
-- time                 14.37 ms   (14.27 ms .. 14.51 ms)
--                      1.000 R²   (0.999 R² .. 1.000 R²)
-- mean                 14.35 ms   (14.31 ms .. 14.44 ms)
-- std dev              140.5 μs   (71.31 μs .. 264.7 μs)


-- Case                             Allocated  GCs
-- not fused binary tree test     239,074,040   53
-- build fused binary tree test    45,539,976   10
-- stream fused binary tree test   22,544,128    5