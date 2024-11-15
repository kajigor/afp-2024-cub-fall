-- NOTE: I don't understand why without this generated log function signatures
-- are illegal bc I don't touch kind annotations. Should I replace all `KindedTV`
-- with `PlainTV`?
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module GenLogTest where

import GenLog

test :: Int -> String -> Bool
test x y = x > 0 && y == "test"

testIO :: Bool -> Int -> IO Int
testIO b x = do
  if b then putStrLn "True" else putStrLn "False"
  return $ x + 1

$(genLogFuncs ['test, 'testIO])

data Wrapper a = Wrapper a | DoubleWrapper a a deriving (Show)

-- Existing constraints like `Num b` are preserved
testPolyConstraints :: (Num b) => Wrapper a -> b -> [b] -> [[b]] -> b
testPolyConstraints _ x _ _ = x + x

$(genLogFuncs ['testPolyConstraints])

data Redundant a = Redundant deriving (Show)

testRedundant :: Redundant a -> Int
testRedundant _ = 42

-- Note that generated function can ask for redundant constraints
-- Because it is hard to analyze what constraints are actually needed
$(genLogFuncs ['testRedundant])

main :: IO ()
main = do
  putStrLn "Testing test"
  _ <- testLog 1 "test"
  putStrLn "Testing testIO"
  _ <- testIOLog True 1
  putStrLn "Testing testPolyConstraints"
  _ <- testPolyConstraintsLog (Wrapper 1) 1 [2] [[3], [4]]
  return ()