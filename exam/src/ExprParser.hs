module ExprParser where

import Control.Applicative
import Data.Char
import Parser
import Interpreter (Expr(..), BinaryOp (..), runExpr, UnaryOp (..))

-- Parsing numbers
number :: Parser Float
number = read <$> some (satisfy isDigit) <* space

-- Parsing numbers
float :: Parser Float
float =read <$> 
  do
    x <- some (satisfy isDigit) 
    _ <- char '.' 
    y <- some (satisfy isDigit) 
    space
    return $ x ++ "." ++ y
  <|> number

-- Parsing operators
symbol :: Char -> Parser Char
symbol c = char c <* space

parens :: Parser a -> Parser a
parens p = symbol '(' *> p <* symbol ')'

term :: Parser (Expr Float)
term = parens expr <|> Val <$> float 
  <|> (UnOp Tan <$> (string "tg" *> parens expr)) 
  <|> (UnOp Sin <$> (string "sin" *> parens expr))
  <|> Var <$> some (satisfy isAlpha)

factor :: Parser (Expr Float)
factor = term `chainl1` (BinOp Mul <$ symbol '*' <|> BinOp Div <$ symbol '/')

expr :: Parser (Expr Float)
expr = factor `chainl1` (BinOp Add <$ symbol '+' <|> BinOp Sub <$ symbol '-')

-- Top-level function to parse an expression
parseExpression :: String -> Maybe (Expr Float)
parseExpression input = case runParser (expr <* eof) input of
  [result] -> Just result
  _              -> Nothing

runWhole s = runExpr <$> parseExpression s