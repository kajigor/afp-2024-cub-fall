{-# LANGUAGE DeriveFunctor #-}

module FL where

-- Free Monad for the 2nd task --
import Control.Monad.Free

data TSize = H1 | H2 | H3

type UiItem = Free UiItemFunctor ()

-- describe your DSL here --
data UiItemFunctor next
    = Title TSize String next
    | Paragraph String next
    | Image String String next
    | Link String String next 
    | OrdL UiItem next 
    | UnordL UiItem next 
    | Input String next  
    | Form String UiItem next 
    | Button String next 
    deriving (Functor)