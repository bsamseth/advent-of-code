-- stack --resolver lts-16.27 script --package containers --optimize

import qualified Data.Set                      as Set

main :: IO ()
main = do
  content <- readFile "input.txt"
  let strings = lines content
  putStrLn $ "Part 1: " ++ (show . countNice isNice1) strings
  putStrLn $ "Part 2: " ++ (show . countNice isNice2) strings

countNice :: (String -> Bool) -> [String] -> Int
countNice nicePredicate = length . filter nicePredicate

isNice1 :: String -> Bool
isNice1 s = and [hasThreeVowels s, hasDoubleLetter s, hasNoIllegalPair s]

isNice2 :: String -> Bool
isNice2 s = and [hasRepeating s, hasRepeatedPair s]

hasThreeVowels :: String -> Bool
hasThreeVowels s = ((>= 3) . length . filter isVowel) s
  where isVowel c = elem c "aeiou"

hasDoubleLetter :: String -> Bool
hasDoubleLetter (x : y : xs) = or [x == y, hasDoubleLetter (y : xs)]
hasDoubleLetter _            = False

hasNoIllegalPair :: String -> Bool
hasNoIllegalPair (x : y : xs) = and
  [ (not . elem (x : y : [])) ["ab", "cd", "pq", "xy"]
  , hasNoIllegalPair (y : xs)
  ]
hasNoIllegalPair _ = True

hasRepeating :: String -> Bool
hasRepeating (x : y : z : xs) = or [x == z, hasRepeating (y : z : xs)]
hasRepeating _                = False

hasRepeatedPair :: String -> Bool
hasRepeatedPair s = search s (Set.fromList [])
 where
  search (x : y : xs) seen = or
    [ Set.member (x : y : []) seen
    , search xs       (Set.insert (x : y : []) seen)
    , search (y : xs) seen
    ]
  search _ _ = False
