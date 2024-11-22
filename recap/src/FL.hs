{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}

module FL where

-- Free Monad for the 2nd task --
import Control.Monad.Free
import Control.Monad.Writer
import Text.Printf (printf)
import Control.Monad (void)

data TitleLevel = H1 | H2 | H3 deriving Show

-- data Input = Input { inputType :: String, value :: String }

type Html a = Free HtmlF a

-- describe your DSL here --
data HtmlF a
  = Header TitleLevel String a
  | Paragraph String a
  | Image String a
  | OrderedList a a
  | UnorderedList (Html ()) a
  | Li String a
  | Form String (Html ()) a
  | Input String String a
  deriving Functor

-- deriving instance Show a => Show (UiItem a)

-- instance Functor UiItem where
--   fmap :: (a -> b) -> UiItem a -> UiItem b
--   fmap f (Header x y z) = Header x y (f z)
--   fmap f (Paragraph x y) = Paragraph x (f y)
--   fmap f (Image x y) = Image x (f y)
--   fmap f (OrderedList x y) = OrderedList (f x) (f y)
--   fmap f (UnorderedList x y) = UnorderedList x (f y)
--   fmap f (Li x y) = Li x (f y)
--   fmap f (Form x y z) = Form x y (f z)
--   fmap f (Input x y z) = Input x y (f z)


title :: TitleLevel -> String -> Html ()
title level name = liftF $ Header level name ()

ul :: Html () -> Html ()
ul body = Free $ UnorderedList body $ Pure ()

li :: String -> Html ()
li content = liftF $ Li content ()

p :: String -> Html ()
p content = liftF $ Paragraph content ()

doc :: Html ()
doc = do
    title H1 "The story about little test"
    ul $ do
        li "The test was able to test unordered list"
        li "And not even with one element"
    p "Moreover, it could even check if paraphs are supported"
    title H3 "The End"

-- implement interpreters --
render :: Html a -> IO ()
render = undefined

showH :: TitleLevel -> String -> Writer String ()
showH x s = case x of
    H1 -> helper "h1"
    H2 -> helper "h2"
    H3 -> helper "h3"
    where
        helper :: String -> Writer String ()
        helper h = tag h (tell s)

-- writeToConsole1 :: Html () -> Writer String ()
-- writeToConsole1 (Pure _) = pure ()
-- writeToConsole1 (Free val) = void (interpret val)

tag :: String -> Writer String () -> Writer String ()
tag name body = tell (printf "<%s>" name) >> body >> tell (printf "</%s>" name)

-- go :: HtmlF (Html ()) -> Writer String ()
-- go (Header level name a) = showH level name >> writeToConsole1 a
-- go (Paragraph content a) = tag "p" (tell content) >> writeToConsole1 a
-- go (UnorderedList content a) = tag "ul" (writeToConsole1 content) >> writeToConsole1 a
-- go (Li content a) = tag "li" (tell content) >> writeToConsole1 a
-- go _ = undefined

interpret :: Html () -> Writer String ()
interpret = foldFree go
    where
        go :: HtmlF x -> Writer String x
        go (Header level name a) = showH level name >> pure a
        go (Paragraph content a) = tag "p" (tell content) >> pure a
        go (UnorderedList content a) = tag "ul" (interpret content) >> pure a
        go (Li content a) = tag "li" (tell content) >> pure a
        go _ = undefined

writeToConsole :: Writer String () -> IO ()
writeToConsole w = putStr $ snd $ runWriter w

writeToConsole11 :: Html () -> IO ()
writeToConsole11 x = writeToConsole (interpret x)

-- hh :: Hdl () -> IO ()
-- hh f = putStr $ snd $ runWriter $ foldFree go f
