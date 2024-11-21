module FL
  ( UiComponent,
    plain,
    path,
    url,
    title1,
    title2,
    title3,
    paragraph,
    image,
    link,
    orderedList,
    unorderedList,
    input,
    button,
    form,
    renderHTML,
  )
where

-- Free Monad for the 2nd task --
import Control.Monad.Free
import Control.Monad.Writer
import Text.Printf (printf)

-- describe your DSL here --
data UiItem a
  = Plain String a
  | Title TitleSize String a
  | Paragraph String a
  | Image Path String a
  | Link URL String a
  | List Order UiComponent a
  | Input String a
  | Button String a
  | Form String UiComponent a

data Order = Ordered | Unordered

data TitleSize = H1 | H2 | H3

newtype URL = URL String

newtype Path = Path String

url :: String -> URL
url = URL

path :: String -> Path
path = Path

instance Functor UiItem where
  fmap f (Plain s a) = Plain s (f a)
  fmap f (Title ts s a) = Title ts s (f a)
  fmap f (Paragraph s a) = Paragraph s (f a)
  fmap f (Image p s a) = Image p s (f a)
  fmap f (Link p s a) = Link p s (f a)
  fmap f (List o s a) = List o s (f a)
  fmap f (Input s a) = Input s (f a)
  fmap f (Button s a) = Button s (f a)
  fmap f (Form s c a) = Form s c (f a)

type UiComponent = Free UiItem ()

plain :: String -> UiComponent
plain s = liftF $ Plain s ()

title1 :: String -> UiComponent
title1 s = liftF $ Title H1 s ()

title2 :: String -> UiComponent
title2 s = liftF $ Title H2 s ()

title3 :: String -> UiComponent
title3 s = liftF $ Title H3 s ()

paragraph :: String -> UiComponent
paragraph s = liftF $ Paragraph s ()

image :: Path -> String -> UiComponent
image p s = liftF $ Image p s ()

link :: URL -> String -> UiComponent
link u s = liftF $ Link u s ()

orderedList :: UiComponent -> UiComponent
orderedList s = liftF $ List Ordered s ()

unorderedList :: UiComponent -> UiComponent
unorderedList s = liftF $ List Unordered s ()

input :: String -> UiComponent
input s = liftF $ Input s ()

button :: String -> UiComponent
button s = liftF $ Button s ()

form :: String -> UiComponent -> UiComponent
form s c = liftF $ Form s c ()

-- implement interpreters --
renderHTML :: UiComponent -> String
renderHTML comp = snd $ runWriter $ toWriter comp
  where
    toWriter :: UiComponent -> Writer String ()
    toWriter = foldFree transform

    transform :: UiItem a -> Writer String a
    transform (Plain s next) = tell s >> return next
    transform (Title ts s next) = let tss = titleSize ts 
                                  in tell (printf "<%s>%s</%s>\n" tss s tss) >> return next
    transform (Paragraph s next) = tell ("<p>" ++ s ++ "</p>\n") >> return next
    transform (Image (Path p) s next) = tell ("<img src=\"" ++ p ++ "\" alt=\"" ++ s ++ "\">\n") >> return next
    transform (Link (URL u) s next) = tell ("<a href=\"" ++ u ++ "\">" ++ s ++ "</a>\n") >> return next
    transform (List Ordered s next) = tell "<ol>\n" >> listItems s >> tell "</ol>\n" >> return next
    transform (List Unordered s next) = tell "<ul>\n" >> listItems s >> tell "</ul>\n" >> return next
    transform (Input s next) = tell ("<input type=\"text\" value=\"" ++ s ++ "\"></input>\n") >> return next
    transform (Button s next) = tell ("<button type=\"submit\">" ++ s ++ "</button>\n") >> return next
    transform (Form s c next) = tell ("<form action=\"" ++ s ++ "\">\n") >> toWriter c >> tell "</form>\n" >> return next

    titleSize :: TitleSize -> String
    titleSize H1 = "h1"
    titleSize H2 = "h2"
    titleSize H3 = "h3"

    listItems :: UiComponent -> Writer String ()
    listItems = foldFree $ \c -> tell "<li>" >> transform c <* tell "</li>\n" 