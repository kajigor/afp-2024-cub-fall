module Parser where

import Control.Applicative
import Data.Char
import Control.Monad.State (StateT (..))
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

-- Parsing numbers
number :: Parser Int
number = read <$> some (satisfy isDigit) <* space

-- Parsing operators
symbol :: Char -> Parser Char
symbol c = char c <* space

-- Expression parser
data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Val Int
          deriving (Show)

parens :: Parser a -> Parser a
parens p = symbol '(' *> p <* symbol ')'

term :: Parser Expr
term = parens expr <|> Val <$> number

factor :: Parser Expr
factor = term `chainl1` (Mul <$ symbol '*' <|> Div <$ symbol '/')

expr :: Parser Expr
expr = factor `chainl1` (Add <$ symbol '+' <|> Sub <$ symbol '-')

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest x = (do
        f <- op
        y <- p
        rest (f x y)) <|> pure x

runParser :: Parser a -> String -> [a]
runParser p s = fst <$> runStateT p s

-- Top-level function to parse an expression
parseExpression :: String -> Maybe Expr
parseExpression input = case runStateT (expr <* eof) input of
  [(result, "")] -> Just result
  _              -> Nothing

-- End of input parser
eof :: Parser ()
eof = StateT $ \input -> case input of
  "" -> [((), "")]
  _   -> []
