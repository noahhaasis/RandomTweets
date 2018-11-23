import qualified Data.HashMap.Strict as Map
import Parser

-- TODO: Move the generator in a seperate module

type Transition = (Word', Word')
transitions :: [Word'] -> [Transition]
transitions ws = zip (Start:ws) (ws ++ [End])

probabilityOfElementsInList :: [a] -> [(a, Rational)]
probabilityOfElementsInList = undefined

type TransitionProbabilityMap = Map.HashMap Word' (Word', Rational)
toTransitionProbabilityMap :: [Transition] -> TransitionProbabilityMap
toTransitionProbabilityMap ts = undefined
  where nextWordMap = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) ts

nextWord :: TransitionProbabilityMap -> Word' -> Word'
nextWord tbs w = undefined

generateTextFromProbabilities :: TransitionProbabilityMap -> [Char]
generateTextFromProbabilities tbs = undefined

generateText :: String -> String
generateText t = undefined

main :: IO ()
main = do
  text <- parseTextFromFile "test.txt"
  return ()
