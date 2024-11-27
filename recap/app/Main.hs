module Main (main) where

import FL (UiItem (..))
import SmartConstructors
import Render 

-- eDSL --
uiComponent = do 
  h1 "Start of HTML file"
  p "Lists" 
  ol $ do 
    h3 "First line"
    img "path" "text"
  ul $ do 
    h3 "First line"
    a "link" "text"
  p "Forms"
  form "Form1" $ do 
    h3 "input1"
    input "enter"
    h3 "input2"
    input "enter"
    button "submit"
  h1 "End of HTML file"


-- for example, render is the name of your interpreter --
main :: IO ()
main = do
  putStrLn $ render uiComponent
