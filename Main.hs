{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import qualified Data.HashMap.Strict as Map
import qualified Data.List as List
import qualified Control.Monad.Random as Random
import Data.Maybe
import Control.Monad.Trans.Maybe
import Parser

-- TODO: Move the generator into a seperate module
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

wordList :: forall m. Random.MonadRandom m => TransitionProbabilityMap -> Word' -> m (Maybe [Word'])
wordList tbm w = doubleSequence $ iterate liftedNextWord (pure $ Just w)
  where liftNext :: (Word' -> m (Maybe Word')) -> (m (Maybe Word') -> m (Maybe Word'))
          = \f -> runMaybeT . (>>= (MaybeT . f)) . MaybeT
        liftedNextWord :: m (Maybe Word') -> m (Maybe Word')
          = liftNext $ nextWord tbm
        doubleSequence ::[m (Maybe Word')] -> m (Maybe [Word'])
          = (sequence <$>) . (takeWhile isJust <$>) . sequence

concatWords :: [Word'] -> String
concatWords = foldr (\a b -> wordToStr a ++ b) ""
  where wordToStr (Word' s) = s
        wordToStr _         = ""

generateText :: String -> m (Maybe String)
generateText t = undefined

main :: IO ()
main = return ()
