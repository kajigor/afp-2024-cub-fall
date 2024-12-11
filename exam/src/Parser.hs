module Parser where

import Control.Applicative
import Data.Char
import Control.Monad (void)
import Control.Monad.State.Lazy
import Data.List (uncons)

-- Basic Parser definition
-- data Parser a = Parser { runParser :: String -> [(a, String)] }
type Parser a = StateT String [] a

-- Basic combinators
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
    s <- get
    case uncons s of
        Just (c, s') -> put s' >> if f c then return c else empty
        Nothing -> empty

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string = traverse char

space :: Parser ()
space = void (many (satisfy isSpace))

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest x = (do
        f <- op
        y <- p
        rest (f x y)) <|> pure x

eof :: Parser ()
eof = StateT $ \input -> case input of
  "" -> [((), "")]
  _   -> []

runParser :: Parser a -> String -> [a]
runParser p s = fst <$> runStateT p s
