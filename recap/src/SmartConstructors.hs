module SmartConstructors where 

import FL 
import Control.Monad.Free

h :: TSize -> String -> UiItem
h tsize text = liftF $ Title tsize text ()

h1 :: String -> UiItem
h1 = h H1 

h2 :: String -> UiItem
h2 = h H2 

h3 :: String -> UiItem
h3 = h H3 

p :: String -> UiItem
p text = liftF $ Paragraph text ()

ol :: UiItem -> UiItem 
ol body = liftF $ OrdL body () 

ul :: UiItem -> UiItem 
ul body = liftF $ UnordL body () 

img :: String -> String -> UiItem
img path text = liftF $ Image path text ()

a :: String -> String -> UiItem
a link text = liftF $ Link link text ()

input :: String -> UiItem
input text = liftF $ Input text ()

form :: String -> UiItem -> UiItem 
form text body = liftF $ Form text body ()

button :: String -> UiItem
button text = liftF $ Button text ()
