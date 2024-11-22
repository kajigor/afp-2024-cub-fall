{-# LANGUAGE DeriveFunctor #-}

module FL (HtmlF (..), TitleLevel (..), Html) where

-- Free Monad for the 2nd task --
import Control.Monad.Free

data TitleLevel = H1 | H2 | H3 deriving (Show)

type Html a = Free HtmlF a

-- describe your DSL here --
data HtmlF a
  = Header TitleLevel String a
  | Paragraph String a
  | Image String a
  | OrderedList (Html ()) a
  | UnorderedList (Html ()) a
  | Li String a
  | Form String (Html ()) a
  | Input String String a
  deriving (Functor)
