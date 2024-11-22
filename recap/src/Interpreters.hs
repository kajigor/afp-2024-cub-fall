module Interpreters (render) where

import Control.Monad.Free (foldFree)
import Control.Monad.Writer
import FL
import Text.Printf (printf)

showH :: TitleLevel -> String -> Writer String ()
showH x s = case x of
  H1 -> helper "h1"
  H2 -> helper "h2"
  H3 -> helper "h3"
  where
    helper :: String -> Writer String ()
    helper h = tag h (tell s)

tag :: String -> Writer String () -> Writer String ()
tag name = tagWithAttrs name []

tagWithAttrs :: String -> [(String, String)] -> Writer String () -> Writer String ()
tagWithAttrs name attrs body = tell (printf "<%s%s>" name (showAttrs attrs)) >> body >> tell (printf "</%s>" name)
  where
    showAttrs :: [(String, String)] -> String
    showAttrs = concatMap (\(attrName, attrValue) -> " " ++ attrName ++ "=" ++ show attrValue)

interpret :: Html a -> Writer String a
interpret = foldFree go
  where
    go :: HtmlF x -> Writer String x
    go x = case x of
      (Header level name a) -> showH level name >> pure a
      (Paragraph content a) -> tag "p" (tell content) >> pure a
      (UnorderedList body a) -> tag "ul" (interpret body) >> pure a
      (OrderedList body a) -> tag "ol" (interpret body) >> pure a
      (Li content a) -> tag "li" (tell content) >> pure a
      (Form action body a) -> tagWithAttrs "form" [("action", action), ("method", "post")] (interpret body) >> pure a
      (Input t value a) -> tagWithAttrs "input" [("type", t), ("value", value)] (tell "") >> pure a
      (Image src a) -> tagWithAttrs "img" [("src", src)] (tell "") >> pure a

writeToConsole :: Writer String a -> IO ()
writeToConsole w = putStrLn $ snd $ runWriter w

-- implement interpreters --
render :: Html a -> IO ()
render x = writeToConsole (interpret x)
