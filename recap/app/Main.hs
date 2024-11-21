module Main (main) where

import FL

-- eDSL --
uiComponent :: UiComponent
uiComponent = do
  title1 "Hello, World!"
  paragraph "This is a simple example of a Free Monad."
  image (path "path/to/image") "Image description"
  link (url "https://github.com") "GitHub"
  orderedList $ do 
    plain "First item"
    orderedList $ do
      plain "First inner item"
      plain "Second inner item"
    plain "Third item"
  unorderedList $ do
    plain "First item"
    plain "Second item"
  form "/test/action" $ do 
    input "Text1"
    button "Submit"
    input "Text2"
    button "Submit2"

-- for example, render is the name of your interpreter --
main :: IO ()
main = putStrLn $ renderHTML uiComponent
