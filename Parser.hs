{-# LANGUAGE DeriveGeneric #-}
module Parser (Word' (..), parseTextFromFile) where
import Text.ParserCombinators.Parsec
import Data.Hashable
import GHC.Generics

data Word' = Word' String | Start | End
             deriving (Show, Eq, Generic)

instance Hashable Word'

punctuationChars = ";,:-\"'?!."
alphaNumChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

whitespace :: Parser Char
whitespace = noneOf (punctuationChars ++ alphaNumChars)

punctuation :: Parser Word'
punctuation = (Word' . (:[])) <$> (oneOf punctuationChars)

word :: Parser Word'
word = punctuation <|> (Word' <$> (many1 $ oneOf alphaNumChars))

parseWordAndSkip :: Parser Word'
parseWordAndSkip = word <* skipMany whitespace

-- Parse a text into a list of Word'. Punctuation is considered a word.
-- A sequence of alphanumeric characters is a word. Everything else is ignored.
text :: Parser [Word']
text =  do
  words <- many parseWordAndSkip
  return (words)

parseTextFromFile :: String -> IO (Either ParseError [Word'])
parseTextFromFile f = parseFromFile text f
