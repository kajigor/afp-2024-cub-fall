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
  -- func "foldr/build fused binary tree test" fusedBinaryTreeFoldrBuildTest sampleBinaryTree -- uncomment this line when you implement the foldr/build version
  -- func "stream fused binary tree test" fusedBinaryTreeStreamTest sampleBinaryTree -- uncomment this line when you implement the stream version

main :: IO ()
main = do
    defaultMain [
        bench "not fused binary tree test" $ whnf binaryTreeTest sampleBinaryTree
        -- , bench "foldr/buils fused binary tree test" $ whnf fusedBinaryTreeFoldrBuildTest sampleBinaryTree -- uncomment this line when you implement the foldr/build version
        -- , bench "stream fused binary tree test" $ whnf fusedBinaryTreeStreamTest sampleBinaryTree -- uncomment this line when you implement the stream version
        ]
    mainWith memoryBenchmarks