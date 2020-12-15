module Language.Tosa.Parser (

) where

import Language.Tosa (Expression(..))
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T

type Parser = Parsec Void T.Text

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "#")
  (L.skipBlockCommentNested "(#" "#)")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

atom :: String -> Parser a -> Parser a
atom lbl = label lbl . lexeme

nameWord :: Parser T.Text
nameWord = T.cons <$> satisfy isStart <*> takeWhileP Nothing isEnd
    where
        isStart c = isAsciiUpper c || isAsciiLower c || any (==c) ("_!$%&-=^~|\\/*?<>" :: [Char])
        isEnd c = isStart c || isDigit c

lexName :: Parser Expression
lexName = Name <$> atom "name" nameWord

parseQuote :: Parser Expression
parseQuote = Quote <$> between (char '(') (char ')') parseExpression

parseProgram :: Parser Expression
parseProgram = Program <$> between (string "<<") (string ">>") (many parseExpression)

parseExpression :: Parser Expression
parseExpression = try parseProgram <|> try parseQuote <|> lexName



