module Main (main) where

import FL (UiItem, render)

-- eDSL --
uiComponent = undefined


-- for example, render is the name of your interpreter --
main :: IO ()
main = do
  render uiComponent
