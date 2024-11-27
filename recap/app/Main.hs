module Main (main) where

import FL 

-- eDSL --
uiComponent :: UiItem
uiComponent = do
  title "New title"
  paragraph "This is a paragraph."
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
  row $ do
    column $ do
      title "New title column 2"
    column $ do
      image "path"
    column $ do
      title "New title column 3"
                           

-- for example, render is the name of your interpreter --
main :: IO ()
main = do render uiComponent
