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
                | DivStyle String (UiItemMonad a) a -- <div style="display: flex; flex-direction: column;">
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

row :: UiItem -> UiItem
row body = Free $ Pure <$> DivStyle "row" body ()

column :: UiItem -> UiItem
column body = Free $ Pure <$> DivStyle "column" body ()

-- implement interpreters --
render ::  UiItemMonad () -> IO ()
render = foldFree renderUitem

renderUitem :: UiItemF a -> IO a
renderUitem (Title str a) = addTagOnly "h1" (printf str) >> return a
renderUitem (Paragraph str a) = addTagOnly "p" (printf str) >> return a
renderUitem (Link l txt a) = addTag "a" (genTokens ["href", "target"] [l, "_blank"]) (printf txt) >> return a
renderUitem (Text str a) = printf str >> return a
renderUitem (LiTag m a) = addTagOnly "li" (foldFree renderUitem m) >> return a
renderUitem (OrderedList lst a) = addTagOnly "ol" (foldFree renderUitem lst) >> return a
renderUitem (UnOrderedList lst a) = addTagOnly "ul" (foldFree renderUitem lst) >> return a
renderUitem (PostForm url body a) = addTag "form" (genTokens ["action", "method"] [url, "POST"]) (foldFree renderUitem body) >> return a
renderUitem (Images str a) = printf "<img src=" >> printf str >> printf ">" >> return a
renderUitem (InputForm txt a) = printf "<input type=\"text\" value=" >> printf txt >> printf ">\n" >> return a
renderUitem (Button cur_title a) = addTag "button" (genTokens ["type"] ["submit"]) (printf cur_title) >> return a
renderUitem (DivStyle rctype body a) = addTag "div" (genTokens ["style"] [curstyle]) (foldFree renderUitem body) >> return a
    where curstyle :: String
          curstyle = "display: flex; flex-direction:" ++ rctype

addTagOnly :: String -> IO a -> IO a
addTagOnly tag = addTag tag ""

addTag :: String -> String -> IO a -> IO a
addTag tag tokens mon = do
    printf ("<" ++ tag ++ tokens ++ ">")
    result <- mon
    printf ("</" ++ tag ++ ">")
    return result

genTokens :: [String] -> [String] -> String
genTokens labels values = unwords $ zipWith (\l v -> " " ++ l ++ "=\"" ++ v ++ "\"") labels values

