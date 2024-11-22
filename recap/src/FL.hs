{-# LANGUAGE DeriveFunctor #-}

module FL (HtmlF (..), TitleLevel (..), Html, render) where

-- Free Monad for the 2nd task --
import Control.Monad.Free
import Control.Monad.Writer
import Text.Printf (printf)

data TitleLevel = H1 | H2 | H3 deriving (Show)

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
  deriving (Functor)

showH :: TitleLevel -> String -> Writer String ()
showH x s = case x of
  H1 -> helper "h1"
  H2 -> helper "h2"
  H3 -> helper "h3"
  where
    helper :: String -> Writer String ()
    helper h = tag h (tell s)

tag :: String -> Writer String () -> Writer String ()
tag name body = tell (printf "<%s>" name) >> body >> tell (printf "</%s>" name)

interpret :: Html a -> Writer String a
interpret = foldFree go
  where
    go :: HtmlF x -> Writer String x
    go x = case x of
      (Header level name a) -> showH level name >> pure a
      (Paragraph content a) -> tag "p" (tell content) >> pure a
      (UnorderedList content a) -> tag "ul" (interpret content) >> pure a
      (Li content a) -> tag "li" (tell content) >> pure a
      _ -> undefined

writeToConsole :: Writer String a -> IO ()
writeToConsole w = putStrLn $ snd $ runWriter w

-- implement interpreters --
render :: Html a -> IO ()
render x = writeToConsole (interpret x)
