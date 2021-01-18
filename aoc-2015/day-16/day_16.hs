-- stack --resolver lts-16.27 script --optimize --package regex-pcre,split

import           Data.List.Split                ( splitOn )
import           Text.Regex.PCRE

main :: IO ()
main = do
  content <- readFile "input.txt"
  let sues = map parseSue $ lines content
  let
    known =
      parseSue
        "Sue 0: children: 3 cats: 7 samoyeds: 2 pomeranians: 3 akitas: 0 vizslas: 0 goldfish: 5 trees: 3 cars: 2 perfumes: 1"
  let part1 = sueId $ head $ filter (matchSue known) sues
  putStrLn $ "Part 1: " ++ (show part1)


data Sue = Sue
  { sueId   :: Int
  , sueInfo :: [(String, Int)]
  }
  deriving Show

matchSue :: Sue -> Sue -> Bool
matchSue (Sue i y) (Sue j (x : xs)) | elem x y  = matchSue (Sue i y) (Sue j xs)
                                    | otherwise = False
matchSue _ _ = True

parseSue :: String -> Sue
parseSue s = Sue n pairs
 where
  n     = read $ (splitOn " " (s =~ "Sue \\d+" :: String)) !! 1
  pairs = map f (getAllTextMatches $ s =~ "\\w+: \\d+" :: [String])
   where
    f m = (label, read count) where (label : count : []) = splitOn ": " m
