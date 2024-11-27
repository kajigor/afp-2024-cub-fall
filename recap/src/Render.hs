module Render (render) where 

import FL 
import Control.Monad.Trans.Writer
import Control.Monad.Free

-- implement interpreters --
render :: UiItem -> String 
render = execWriter . foldFree transform

renderTsize :: TSize -> String 
renderTsize H1 = "h1"
renderTsize H2 = "h2"
renderTsize H3 = "h3"

transform :: UiItemFunctor a -> Writer String a
transform (Title tsize text next) = renderHTMLtag (renderTsize tsize) [] (tell text) >> return next
transform (Paragraph text next) = renderHTMLtag "p" [] (tell text) >> return next
transform (Image path text next) = renderHTMLtag "img" [("src", path), ("alt", text)] (return ()) >> return next
transform (Link link text next) = renderHTMLtag "a" [("href", link)] (tell text) >> return next
transform (OrdL body next) = renderHTMLtag "ol" [] (foldFree transform body) >> return next
transform (UnordL body next) = renderHTMLtag "ul" [] (foldFree transform body) >> return next
transform (Input text next) = renderHTMLtag "input" [("type", "text"), ("value", text)] (return ()) >> return next
transform (Form text body next) = renderHTMLtag "form" [("action", text)] (foldFree transform body) >> return next
transform (Button text next) = renderHTMLtag "button" [("type", "submit")] (tell text) >> return next

renderHTMLtag :: String -> [(String, String)] -> Writer String a -> Writer String a
renderHTMLtag tag atts writer =  
    tell ("<" ++ tag ++ concatMap (\(att, val) -> " " ++ att ++ "=" ++ "\"" ++ val ++ "\"") atts ++ ">") *> writer <* tell ("</" ++ tag ++ ">")