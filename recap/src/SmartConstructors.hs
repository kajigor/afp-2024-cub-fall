{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module SmartConstructors where

import FL
import Control.Monad.Free
import Data.List (intercalate)

title :: TitleLevel -> String -> Html
title level name = liftF $ Header level name ()

h1 :: String -> Html
h1 = title H1

h2 :: String -> Html
h2 = title H2

h3 :: String -> Html
h3 = title H3

ul :: Html -> Html
ul body = Free $ UnorderedList body $ Pure ()

ol :: Html -> Html
ol body = Free $ OrderedList body $ Pure ()

li :: String -> Html
li content = liftF $ Li content ()

p :: String -> Html
p content = liftF $ Paragraph content ()

img :: String -> Html
img src = liftF $ Image src ()

form :: String -> Html -> Html
form action body = Free $ Form action body $ Pure ()

input :: String -> String -> Html
input t value = Free $ Input t value $ Pure ()

button :: String -> Html
button value = Free $ Input "submit" value $ Pure ()

divi :: Attrs -> Html -> Html
divi attrs body = Free $ Div attrs body $ Pure ()

divWithStyle :: Attrs -> Html -> Html
divWithStyle attrs body = Free $ Div [("style", toStyle attrs)] body $ Pure ()
  where
    toStyle :: Attrs -> String
    toStyle a = intercalate ";" $ map (\(name, value) -> name ++ ":" ++ value) a

columns :: Html -> Html
columns = divWithStyle [("display", "flex"), ("flex-direction", "row")]

rows :: Html -> Html
rows = divWithStyle [("display", "flex"), ("flex-direction", "column")]
