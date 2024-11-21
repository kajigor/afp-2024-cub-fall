module Main where

import Test.Hspec
import BinSearchTree

main :: IO ()
main = hspec $ do
  describe "isMember" $ do
    it "returns True for an element in the tree" $ do
      let tree = Node (Node Leaf 5 Leaf) 10 (Node Leaf 15 Leaf)
      isMember tree 5 `shouldBe` True
      isMember tree 10 `shouldBe` True
      isMember tree 15 `shouldBe` True
    
    it "returns False for an element not in the tree" $ do
      let tree = Node (Node Leaf 5 Leaf) 10 (Node Leaf 15 Leaf)
      isMember tree 8 `shouldBe` False

  describe "insert" $ do
    it "inserts an element into an empty tree" $ do
      let tree = insert Leaf 10
      isMember tree 10 `shouldBe` True

    it "inserts an element into a non-empty tree" $ do
      let tree = insert (Node Leaf 10 Leaf) 5
      isMember tree 5 `shouldBe` True
      isMember tree 10 `shouldBe` True

    it "does not duplicate an element already in the tree" $ do
      let tree = insert (Node Leaf 10 Leaf) 10
      tree `shouldBe` (Node Leaf 10 Leaf)

  describe "findMin" $ do
    it "finds the minimum element in a tree" $ do
      let tree = Node (Node Leaf 5 Leaf) 10 (Node Leaf 15 Leaf)
      findMin tree `shouldBe` 5

  describe "delete" $ do
    it "deletes a leaf node" $ do
      let tree = Node Leaf 10 Leaf
      delete tree 10 `shouldBe` Leaf

    it "deletes a node with one child" $ do
      let tree = Node (Node Leaf 5 Leaf) 10 Leaf
      delete tree 10 `shouldBe` Node Leaf 5 Leaf

    it "deletes a node with two children" $ do
      let tree = Node (Node (Node Leaf 2 Leaf) 5 Leaf) 10 (Node Leaf 15 Leaf)
      delete tree 10 `shouldBe` Node (Node (Node Leaf 2 Leaf)  5 Leaf) 15 Leaf