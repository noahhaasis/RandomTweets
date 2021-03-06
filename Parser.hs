{-# LANGUAGE DeriveGeneric #-}
module Parser (Word' (..), parseText, parseTextFromFile) where
import Text.ParserCombinators.Parsec
import Data.Hashable
import GHC.Generics

-- TODO: Test if everything still works after adding Punctuation (This is were TDD would have helped a lot)
data Word' = Word' String | Punctuation String | Start | End
             deriving (Show, Eq, Generic)

instance Hashable Word'

punctuationChars = ";,:-\"'?!."
alphaNumChars    = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

whitespace :: Parser Char
whitespace = noneOf (punctuationChars ++ alphaNumChars)

punctuation :: Parser Word'
punctuation = Punctuation <$> many1 (oneOf punctuationChars)

word :: Parser Word'
word = punctuation <|> (Word' <$> (many1 $ oneOf alphaNumChars))

parseWordAndSkip :: Parser Word'
parseWordAndSkip = word <* skipMany whitespace

{- Parse a text into a list of Word'. Punctuation is considered a word.
   A sequence of alphanumeric characters is a word. Everything else is ignored. -}
text :: Parser [Word']
text = many parseWordAndSkip

parseText :: String -> Either ParseError [Word']
parseText = parse text ""

-- TODO: Remove later if not in use
parseTextFromFile :: String -> IO (Either ParseError [Word'])
parseTextFromFile = parseFromFile text
