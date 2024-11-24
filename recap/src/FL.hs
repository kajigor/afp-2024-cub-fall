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
    columns,
    rows,
    renderHTML,
  )
where

-- Free Monad for the 2nd task --
import Control.Monad.Free
import Control.Monad.Writer

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
  | Columns UiComponent a
  | Rows UiComponent a

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
  fmap f (Columns c a) = Columns c (f a)
  fmap f (Rows c a) = Rows c (f a)

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

columns :: UiComponent -> UiComponent
columns c = liftF $ Columns c ()

rows :: UiComponent -> UiComponent
rows c = liftF $ Rows c ()

-- implement interpreters --
renderHTML :: UiComponent -> String
renderHTML comp = execWriter $ toWriter comp
  where
    toWriter :: UiComponent -> Writer String ()
    toWriter = foldFree transform

    transform :: UiItem a -> Writer String a
    transform (Plain s next) = tell s >> return next
    transform (Title ts s next) = tag (titleSize ts) [] (tell s) >> return next
    transform (Paragraph s next) = tag "p" [] (tell s) >> return next
    transform (Image (Path p) s next) = tag "img" [("src", p), ("alt", s)] (return ()) >> return next
    transform (Link (URL u) s next) = tag "a" [("href", u)] (tell s) >> return next
    transform (List Ordered s next) = tag "ol" [] (listItems s) >> return next
    transform (List Unordered s next) = tag "ul" [] (listItems s) >> return next
    transform (Input s next) = tag "input" [("type", "text"), ("value", s)] (return ()) >> return next
    transform (Button s next) = tag "button" [("type", "submit")] (tell s) >> return next
    transform (Form s c next) = tag "form" [("action", s)] (toWriter c) >> return next
    transform (Columns c next) = tag "div" [("style", "display: flex;")] (flexItems c) >> return next
    transform (Rows c next) = tag "div" [("style", "display: flex; flex-direction: column;")] (flexItems c) >> return next

    tag :: String -> [(String, String)] -> Writer String a -> Writer String a
    tag t as w =
      let attrs = concatMap (\(k, v) -> " " ++ k ++ "=\"" ++ v ++ "\"") as
       in tell ("<" ++ t ++ attrs ++ ">") >> w <* tell ("</" ++ t ++ ">\n")

    titleSize :: TitleSize -> String
    titleSize H1 = "h1"
    titleSize H2 = "h2"
    titleSize H3 = "h3"

    listItems :: UiComponent -> Writer String ()
    listItems = foldFree $ tag "li" [] . transform

    flexItems :: UiComponent -> Writer String ()
    flexItems = foldFree $ tag "div" [("style", "flex: 1;")] . transform