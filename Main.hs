{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import qualified Control.Monad.Random       as Random
import           Control.Monad.Trans.Maybe
import qualified Data.HashMap.Strict        as Map
import qualified Data.List                  as List
import           Data.Maybe
import           Parser

{- TODO: Move the generator into a seperate module -}
type Transition = (Word', Word')
type TransitionProbabilityMap = Map.HashMap Word' [(Word', Rational)]

transitions :: [Word'] -> [Transition]
transitions ws = zip (Start:ws) (ws ++ [End])

probabilityOfElementsInList :: Eq a => [a] -> [(a, Rational)]
probabilityOfElementsInList xs = map toProbTuple (List.group xs)
  where len           = toRational $ length xs
        toProbTuple x = (head x, toRational (length x) / len)

toTransitionProbabilityMap :: [Transition] -> TransitionProbabilityMap
toTransitionProbabilityMap ts = Map.map probabilityOfElementsInList nextWordMap
  where nextWordMap = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) ts

nextWord :: Random.MonadRandom m => TransitionProbabilityMap -> Word' -> m (Maybe Word')
nextWord tbm w = sequence $ Random.fromList <$> Map.lookup w tbm

{- TODO: Prevent the list from being infinite -}
randomWordList :: forall m. Random.MonadRandom m => TransitionProbabilityMap -> Word' -> m (Maybe [Word'])
randomWordList tbm w = doubleSequence $ iterate liftedNextWord (pure $ Just w)
  where liftNext :: (Word' -> m (Maybe Word')) -> (m (Maybe Word') -> m (Maybe Word'))
          = \f -> runMaybeT . (>>= (MaybeT . f)) . MaybeT
        liftedNextWord :: m (Maybe Word') -> m (Maybe Word')
          = liftNext $ nextWord tbm
        doubleSequence :: [m (Maybe Word')] -> m (Maybe [Word'])
          = (sequence <$>) . (takeWhile isJust <$>) . sequence

{- TODO: Handle apostrophes correctly. -}
concatWords :: [Word'] -> String
concatWords ws = case concatMap wordToStr ws of
  ' ' : xs -> xs
  xs       -> xs
  where wordToStr (Word' w)       = ' ' : w  -- Prefix all words with a space
        wordToStr (Punctuation p) = p
        wordToStr Start           = ""
        wordToStr End             = ""

generateText :: Random.MonadRandom m => String -> m (Maybe String)
generateText t = case newSentence of
  (Right s) -> fmap concatWords <$> s
  (Left _)  -> pure Nothing
  where
    ws = parseText t
    ts = transitions <$> ws
    tpm = toTransitionProbabilityMap <$> ts
    newSentence = (randomWordList <$> tpm) <*> Right Start

main :: IO ()
main = return ()
