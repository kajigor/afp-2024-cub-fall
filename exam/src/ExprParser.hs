module ExprParser where

import Control.Applicative
import Data.Char
import Parser
import Interpreter (Expr(..), BinaryOp (..), UnaryOp (..))

tok :: Parser a -> Parser a
tok p = p <* many (satisfy isSpace)

varName :: Parser String
varName = tok (some (satisfy isAlpha))

number :: Parser Float
number = read <$> some (satisfy isDigit) <* space

float :: Parser Float
float =read <$>
  do
    x <- some (satisfy isDigit)
    _ <- char '.'
    y <- some (satisfy isDigit)
    space
    return $ x ++ "." ++ y
  <|> number

symbol :: Char -> Parser Char
symbol c = char c <* space

parens :: Parser a -> Parser a
parens p = symbol '(' *> p <* symbol ')'

term :: Parser (Expr Float)
term = parens expr <|> Val <$> float
  <|> (UnOp Tan <$> (string "tg" *> parens expr))
  <|> (UnOp Sin <$> (string "sin" *> parens expr))
  <|> Var <$> varName

factor :: Parser (Expr Float)
factor = term `chainl1` (BinOp Mul <$ symbol '*' <|> BinOp Div <$ symbol '/')

expr :: Parser (Expr Float)
expr = factor `chainl1` (BinOp Add <$ symbol '+' <|> BinOp Sub <$ symbol '-')

varDefinition :: Parser (Expr ())
varDefinition = do
    name <- varName
    _ <- tok $ string ":="
    Let name <$> expr

stmts :: Parser (Expr Float)
stmts = Seq <$> ((varDefinition <* tok (char ';')) `chainl1` return Seq) <*> expr <|> expr

parseExpression :: String -> Maybe (Expr Float)
parseExpression input = case runParser (stmts <* eof) input of
  [result] -> Just result
  _              -> Nothing
