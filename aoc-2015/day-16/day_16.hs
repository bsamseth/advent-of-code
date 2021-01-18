-- stack --resolver lts-16.27 script --optimize --package regex-pcre,split,containers

import           Data.List.Split                ( splitOn )
import qualified Data.Map                      as M
import           Text.Regex.PCRE

main :: IO ()
main = do
  content <- readFile "input.txt"
  let sues = map parseSue $ lines content

  let
    known =
      parseSue
        "Sue 0: children: 3 cats: 7 samoyeds: 2 pomeranians: 3 akitas: 0 vizslas: 0 goldfish: 5 trees: 3 cars: 2 perfumes: 1"
  let part1 = sueId $ head $ filter (matchSue 1 known) sues
  let part2 = sueId $ head $ filter (matchSue 2 known) sues
  putStrLn $ "Part 1: " ++ (show part1)
  putStrLn $ "Part 2: " ++ (show part2)


data Sue = Sue
  { sueId   :: Int
  , sueInfo :: M.Map String Int
  }
  deriving Show

parseSue :: String -> Sue
parseSue s = Sue n $ M.fromList pairs
 where
  n     = read $ (splitOn " " (s =~ "Sue \\d+" :: String)) !! 1
  pairs = map f (getAllTextMatches $ s =~ "\\w+: \\d+" :: [String])
   where
    f m = (label, read count) where (label : count : []) = splitOn ": " m

matchSue :: Int -> Sue -> Sue -> Bool
matchSue part (Sue _ y) (Sue _ x) = and
  [ f part k $ M.lookup k x | k <- M.keys y ]
 where
  vv = (M.!) y
  f 2 "cats"        (Just v) = v > vv "cats"
  f 2 "trees"       (Just v) = v > vv "trees"
  f 2 "pomeranians" (Just v) = v < vv "pomeranians"
  f 2 "goldfish"    (Just v) = v < vv "goldfish"
  f _ k             (Just v) = v == vv k
  f _ _             Nothing  = True
