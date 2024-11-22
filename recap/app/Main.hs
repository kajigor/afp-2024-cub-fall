module Main (main) where

import FL
import Interpreters
import SmartConstructors

-- eDSL --
uiComponent :: Html
uiComponent = do
  h1 "The story about little test"
  ul $ do
    li "The test was able to test unordered lists"
    li "And not even with one element"
  ol $ do
    li "Ordered lists were no exception"
    li "And also with several elements"
  p "Moreover, it could even check if paraphs are rendered correctly"
  h2 "But the test was mostly proud of its capability to test forms"
  form "/delete/all" $ do
    input "text" "John"
    input "text" "Doe"
    button "Submit"
  h3 "Letztlich konnte ein Test auch die Bildern ziehen."
  img "./resources/pretty_picture.png"
  p "The End"

flexUiComponent :: Html
flexUiComponent = do
  h3 "Rows"
  columns $ do
    p "Column 1"
    p "Column 2"
    p "Column 3"
  h3 "Columns"
  rows $ do
    columns $ do
      p "Column 1"
      p "Column 2"
    p "Row 2"
    p "Row 3"

-- for example, render is the name of your interpreter --
main :: IO ()
main = do
  render uiComponent
  putStrLn ""
  render flexUiComponent
