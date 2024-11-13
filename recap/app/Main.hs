module Main (main) where

import Hdl (UiItem(..), TitleLevel(..), SubItemType(..), writeToConsole)

uiComponents = [
  AddTextLine "Simple text",
  AddSubItem (Ordered 1) "Carrot",
  AddSubItem (Ordered 2) "Apple"
  ]

main :: IO [()]
main = do
  traverse writeToConsole uiComponents
