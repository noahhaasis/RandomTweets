module Test where
import Test.Hspec
import Main hiding (main)
import Parser

main :: IO ()
main = hspec $ do
  describe "concatenating words to a sentence" $ do
    it "concatenates Start and End to a empty sentence" $ do
      concatWords [Start, End] `shouldBe` ""
    it "concatenates a single word" $ do
      concatWords [Word' "foo"] `shouldBe` "foo"
    it "concatenates multiple words" $ do
      concatWords [Word' "foo", Word' "bar", Word' "fizz"] `shouldBe` "foo bar fizz"
    it "concatenates a single punctuation" $ do
      concatWords [Punctuation "!"] `shouldBe` "!"
    it "concatenates multiple sentences containing words and punctuation" $ do
      concatWords [Word' "Hello", Punctuation ","
                  , Word' "World", Punctuation "!"
                  , Word' "Hallo", Punctuation "?"] `shouldBe` "Hello, World! Hallo?"
    it "concatenates the empty list to a empty sentence" $ do
      concatWords [] `shouldBe` ""
