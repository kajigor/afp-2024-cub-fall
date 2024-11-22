module SmartConstructors (title, ul, li, p) where

import FL
import Control.Monad.Free

title :: TitleLevel -> String -> Html ()
title level name = liftF $ Header level name ()

ul :: Html () -> Html ()
ul body = Free $ UnorderedList body $ Pure ()

li :: String -> Html ()
li content = liftF $ Li content ()

p :: String -> Html ()
p content = liftF $ Paragraph content ()
