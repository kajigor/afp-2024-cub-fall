module Hdl (
    Hdl,
    UiItem(..),
    TitleLevel(..),
    SubItemType(..)
    ) where


data TitleLevel = H1 | H2 | H3
data SubItemType = Ordered Int | Unordered

data UiItem = AddTitleLine TitleLevel String
    | AddTextLine String
    | AddSubItem SubItemType String
    | AddImageUrl String

type Hdl = [UiItem]


-- interpreter --
writeToConsole :: Hdl -> IO ()
writeToConsole [] = putStr ""
writeToConsole (h:t) = do
    writeToConsole' h
    writeToConsole t


writeToConsole' :: UiItem -> IO ()
writeToConsole' (AddTitleLine level text) = do
    print $ case level of
        H1 -> "# " ++ text
        H2 -> "## " ++ text
        H3 -> "### " ++ text

writeToConsole' (AddTextLine text) = do
    print text

writeToConsole' (AddSubItem (Ordered val) text) = do
    print $ show val ++ ". " ++ text

writeToConsole' (AddSubItem Unordered text) = do
    print $ "* " ++ text

writeToConsole' (AddImageUrl url) = undefined
