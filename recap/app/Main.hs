module Main (main) where

import FL 

-- eDSL --
uiComponent :: UiItem
uiComponent = do
  title "New title"
  paragraph "This is a paragraph."
  image "path"
  link "https://youtu.be/3Uo0JAUWijM?si=lqB9Rh4Cb41vcgeB" "Happy new year"
  orderedList $ do li (text "Hello")
                   li (link "https://www.wikipedia.org" "Wikipedia")
                   li (link "https://www.github.com" "GitHub")
  unOrderedList $ do li (text "Hello2")
                     li (link "https://www.wikipedia.org" "Wikipedia")
                     li (link "https://www.github.com" "GitHub")
  form "/submit-form" $ do 
                      input "name"
                      button "Submit"
                           

-- for example, render is the name of your interpreter --
main :: IO ()
main = do render uiComponent
