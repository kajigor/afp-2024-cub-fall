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
  -- func "stream fused binary tree test" fusedBinaryTreeStreamTest sampleBinaryTree -- uncomment this line when you implement the stream version

main :: IO ()
main = do
    defaultMain [
        bench "not fused binary tree test" $ whnf binaryTreeTest sampleBinaryTree
        , bench "foldr/buils fused binary tree test" $ whnf fusedBinaryTreeFoldrBuildTest sampleBinaryTree -- uncomment this line when you implement the foldr/build version
        -- , bench "stream fused binary tree test" $ whnf fusedBinaryTreeStreamTest sampleBinaryTree -- uncomment this line when you implement the stream version
        ]
    mainWith memoryBenchmarks


-- benchmarking not fused binary tree test
-- time                 320.9 ms   (303.0 ms .. 356.0 ms)
--                      0.998 R²   (0.997 R² .. 1.000 R²)
-- mean                 315.3 ms   (297.6 ms .. 322.6 ms)
-- std dev              12.57 ms   (3.156 ms .. 16.88 ms)
-- variance introduced by outliers: 19% (moderately inflated)

-- benchmarking foldr/buils fused binary tree test
-- time                 14.89 ms   (14.53 ms .. 15.34 ms)
--                      0.997 R²   (0.995 R² .. 1.000 R²)
-- mean                 14.73 ms   (14.65 ms .. 14.91 ms)
-- std dev              282.2 μs   (170.1 μs .. 471.6 μs)


-- Case                            Allocated  GCs
-- not fused binary tree test    239,073,960   53