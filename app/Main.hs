module Main (main) where

import Query

someQuery :: QueryF
someQuery = do
    field "name" (eq $ literal "John")
    field "age" (gt $ number 30)
    Query.or $ do
        field "city" (eq $ literal "New York")
        field "city" (eq $ literal "San Francisco")
    field "education" $ match $ do
        field "degree" (eq $ literal "PhD")
        field "school" (eq $ literal "MIT")

main :: IO ()
main = putStrLn $ toJSON someQuery