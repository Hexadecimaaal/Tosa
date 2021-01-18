module Language.Tosa.Parser 
  ( parseText
  ) where

import Language.Tosa.Core (Expression(..))
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Void
import Data.Bifunctor (first)
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
parseQuote = Quote <$> between (try $ lexeme $ char '(') (lexeme $ char ')') parseExpression

-- >>> parseTest (sc *> parseProgram <* eof) "    { << >> <<< DUP2 DROP2 (# qaq #)}"
-- {<< >> <<< DUP2 DROP2}
--

parseProgram :: Parser Expression
parseProgram = Program <$> between (try $ lexeme $ string "{") (lexeme $ string "}") (many parseExpression)

parseExpression :: Parser Expression
parseExpression = parseProgram <|> parseQuote <|> lexName

parseText :: String -> T.Text -> Either String Expression
parseText fileName input = first errorBundlePretty $
  parse (sc *> parseExpression <* eof) fileName input

