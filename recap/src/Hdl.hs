module Hdl (
    Hdl,
    addTitleLineElement,
    addTextLineElement,
    addOrderedItem,
    addUnorderedItem,
    addImageUrl,
    writeToConsole
    ) where

import Control.Monad.Free

data TitleLevel = H1 | H2 | H3
data SubItemType = Ordered Int | Unordered

-- updated data type --
data UiItem a = AddTitleLine TitleLevel String a
    | AddTextLine String a
    | AddSubItem SubItemType String a
    | AddImageUrl String a

instance Functor UiItem where
    -- apply function for result of previous evaluation a
    fmap f (AddTitleLine level text a) = AddTitleLine level text (f a)
    fmap f (AddTextLine text a) = AddTextLine text (f a)
    fmap f (AddSubItem value text a) = AddSubItem value text (f a)
    fmap f (AddImageUrl url a) = AddImageUrl url (f a)

type Hdl a = Free UiItem a

addTitleLineElement :: String -> String -> Hdl ()
addTitleLineElement size title = case size of
    "big" -> Free (AddTitleLine H1 title (Pure ()))
    "small" -> Free (AddTitleLine H3 title (Pure ()))
    _ -> Free (AddTitleLine H2 title (Pure ()))

addTextLineElement :: String -> Hdl ()
addTextLineElement text = Free (AddTextLine text (Pure ()))

-- Please note, here 2 different constructors to 1 Hdl value constructor
-- It was implemented this way to provide more convinient way for usage
addOrderedItem :: Int -> String -> Hdl ()
addOrderedItem number text = Free (AddSubItem (Ordered number) text (Pure ()))

addUnorderedItem :: String -> Hdl ()
addUnorderedItem text = Free (AddSubItem Unordered text (Pure ()))

addImageUrl :: String -> Hdl ()
addImageUrl url = Free (AddImageUrl url (Pure ()))


-- interpreter --
writeToConsole :: Hdl () -> IO ()
writeToConsole (Pure _) = putStr ""
writeToConsole (Free val) = writeToConsole' val


writeToConsole' (AddTitleLine level text cont) = do
    print $ case level of
        H1 -> "# " ++ text
        H2 -> "## " ++ text
        H3 -> "### " ++ text
    writeToConsole cont

writeToConsole' (AddTextLine text cont) = do
    print text
    writeToConsole cont

writeToConsole' (AddSubItem (Ordered val) text cont) = do
    print $ show val ++ ". " ++ text
    writeToConsole cont

writeToConsole' (AddSubItem Unordered text cont) = do
    print $ "* " ++ text
    writeToConsole cont

writeToConsole' (AddImageUrl url cont) = undefined

