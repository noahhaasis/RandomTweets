import qualified Data.HashMap.Strict as Map
import qualified Data.List as List
import qualified Control.Monad.Random as Random
import Parser

-- TODO: Move the generator in a seperate module

type Transition = (Word', Word')
type TransitionProbabilityMap = Map.HashMap Word' [(Word', Rational)]


transitions :: [Word'] -> [Transition]
transitions ws = zip (Start:ws) (ws ++ [End])

probabilityOfElementsInList :: Eq a => [a] -> [(a, Rational)]
probabilityOfElementsInList xs = map toProbTuple (List.group xs)
  where len = length xs
        toProbTuple = \x -> (head x, toRational $ length x `div` len)

toTransitionProbabilityMap :: [Transition] -> TransitionProbabilityMap
toTransitionProbabilityMap ts = Map.map probabilityOfElementsInList nextWordMap
  where nextWordMap = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) ts

nextWord :: Random.MonadRandom m => TransitionProbabilityMap -> Word' -> m (Maybe Word')
nextWord tbm w = sequence $ Random.fromList <$> (Map.lookup w tbm)

infiniteText :: m [Word']
infiniteText = tail <$> sequence <$> takeWhile justAndNotEnd $ iterate nextWord Start -- TODO
  where justAndNotEnd Nothing = False
        justAndNotEnd Just x = x /= End

generateTextFromProbabilities :: TransitionProbabilityMap -> [Char]
generateTextFromProbabilities = undefined
-- generateTextFromProbabilities tbs = concatWords $ (takeWhile (/= End)) <$> infiniteText
--   where infiniteText = sequence $ tail $ iterate nextWord Start
--         concatWords = undefined

generateText :: String -> String
generateText t = undefined

main :: IO ()
main = do
  text <- parseTextFromFile "test.txt"
  return ()
