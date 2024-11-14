module Main (main) where

import Hdl (Hdl, addTextLineElement, addOrderedItem, writeToConsole)


uiComponents = 
  addTextLineElement "Simple text" ++
  addOrderedItem 1 "Carrot" ++
  addOrderedItem 2 "Apple" ++
  addOrderedItem 3 "Something else"


main :: IO ()
main = do
  writeToConsole uiComponents
