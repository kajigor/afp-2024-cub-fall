module Main (main) where

import Hdl (Hdl, 
    addTitleLineElement,
    addTextLineElement,
    addOrderedItem,
    addUnorderedItem,
    addImageUrl,
    writeToConsole
    )

uiComponents = do
  addTitleLineElement "small" "Main Title"
  addTextLineElement "Simple text"
  addOrderedItem 1 "Carrot"
  addOrderedItem 2 "Apple"


main :: IO ()
main = do
  writeToConsole uiComponents
