module Main (main) where

import FL 
import SmartConstructors

-- eDSL --
uiComponent :: Html ()
uiComponent = do
    title H1 "The story about little test"
    ul $ do
        li "The test was able to test unordered list"
        li "And not even with one element"
    p "Moreover, it could even check if paraphs are supported"
    title H3 "The End"

-- for example, render is the name of your interpreter --
main :: IO ()
main = do
  render uiComponent
