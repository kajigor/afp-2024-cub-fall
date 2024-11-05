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

main :: IO ()
main = do
  putStrLn "Testing test"
  _ <- testLog 1 "test"
  putStrLn "Testing testIO"
  _ <- testIOLog True 1
  return ()