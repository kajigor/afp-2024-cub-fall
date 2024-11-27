{-# LANGUAGE DeriveFunctor #-}

module FL where

-- Free Monad for the 2nd task --
import Control.Monad.Free
import Text.Printf (printf)


type UiItemMonad a = Free UiItemF a
type UiItem = UiItemMonad ()

-- describe your DSL here --
data UiItemF a = Title String a       -- <h1>HTML Elements Reference</h1>
                | Paragraph String a -- <p>This is a paragraph.</p>
                | Images String a    -- <img src="img_chania.jpg" alt="Flowers in Chania"> 
                | Link String String a-- <a href="https://youtu.be/3Uo0JAUWijM?si=lqB9Rh4Cb41vcgeB">Happy new year</a>
                | LiTag (UiItemMonad a) a
                | Text String a
                | OrderedList (UiItemMonad a) a -- <ol> <li>Coffee</li> </ol>
                | UnOrderedList (UiItemMonad a) a -- <ul> <li>List Item 1</li> </ul>
                | InputForm String a -- <input type="text" value="..."> 
                | PostForm String (UiItemMonad a) a -- <form action="/submit-form" method="POST"> 
                | Button String a --  <button type="submit">Submit</button>
                deriving (Functor)

title :: String -> UiItem
title str = Free $ Pure <$> Title str ()

paragraph :: String -> UiItem
paragraph str = Free $ Pure <$> Paragraph str ()

image :: String -> UiItem
image path = Free $ Pure <$> Images path ()

link :: String -> String -> UiItem
link l txt = Free $ Pure <$> Link l txt ()

text :: String -> UiItem
text txt =  Free $ Pure <$> Text txt ()

li :: UiItem -> UiItem
li content = Free $ Pure <$> LiTag content ()

orderedList :: UiItem -> UiItem
orderedList items = Free $ Pure <$> OrderedList items ()

unOrderedList :: UiItem -> UiItem
unOrderedList items = Free $ Pure <$> UnOrderedList items ()

form :: String -> UiItem -> UiItem
form url body = Free $ Pure <$> PostForm url body ()

input :: String -> UiItem
input txt = Free $ Pure <$> InputForm txt ()

button :: String -> UiItem
button url = Free $ Pure <$> Button url ()

-- implement interpreters --
render ::  UiItemMonad () -> IO ()
render = foldFree renderUitem

renderUitem :: UiItemF a -> IO a
renderUitem (Title str a) = printf "<h1>" >> printf str >> printf "</h1>\n" >> return a
renderUitem (Paragraph str a) = printf "<p>" >> printf str >> printf "</p>\n" >> return a
renderUitem (Images str a) = printf "<img src=" >> printf str >> printf ">\n" >> return a
renderUitem (Link l txt a) = printf "<a href=" >> printf l >> printf " target=\"_blank\">" >> printf txt >> printf "</a>\n" >> return a
renderUitem (Text str a) = printf str >> return a
renderUitem (LiTag m a) = printf "<li>" >> foldFree renderUitem m >> printf "</li>\n" >> return a
renderUitem (OrderedList lst a) = printf "<ol>" >> foldFree renderUitem lst >> printf "</ol>\n" >> return a
renderUitem (UnOrderedList lst a) = printf "<ul>" >> foldFree renderUitem lst >> printf "</ul>\n" >> return a
renderUitem (InputForm txt a) = printf "<input type=\"text\" value=" >> printf txt >> printf ">\n" >> return a
renderUitem (PostForm url body a) = printf "<form action="
                                    >> printf url
                                    >> printf " method=\"POST\">\n" 
                                    >> foldFree renderUitem body
                                    >> printf "</form>\n" >> return a
renderUitem (Button cur_title a) = printf "<button type=\"submit\">" >> printf cur_title >> printf "</button>\n" >> return a

