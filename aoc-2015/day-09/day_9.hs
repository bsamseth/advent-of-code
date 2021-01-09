-- stack --resolver lts-16.27 script --optimize --package split,containers

import           Data.List                      ( permutations )
import           Data.List.Split                ( splitOn )
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

main :: IO ()
main = do
  content <- readFile "input.txt"
  let parsedLines = map parseLine $ lines content
  let distMap     = distanceMap parsedLines
  let locations   = allLocations parsedLines
  let routeLengths =
        [ distanceOfRoute distMap perm
        | perm <- permutations (Set.toList locations)
        ]
  putStrLn $ "Part 1: " ++ show (minimum routeLengths)
  putStrLn $ "Part 2: " ++ show (maximum routeLengths)


distanceMap :: [(String, String, Int)] -> Map.Map (String, String) Int
distanceMap parsedLines =
  Map.fromList $ map toMapPair parsedLines ++ map toMapPairReverse parsedLines
 where
  toMapPair (f, t, dist) = ((f, t), dist)
  toMapPairReverse (f, t, dist) = ((t, f), dist)

parseLine :: String -> (String, String, Int)
parseLine s = (f, t, read rhs)
 where
  (lhs : rhs : []) = splitOn " = " s
  (f   : t   : []) = splitOn " to " lhs

allLocations :: [(String, String, Int)] -> Set.Set String
allLocations ((f, t, _) : xs) =
  Set.union (Set.fromList [f, t]) (allLocations xs)
allLocations [] = Set.empty

distanceOfRoute :: Map.Map (String, String) Int -> [String] -> Int
distanceOfRoute d (f : t : xs) = (d Map.! (f, t)) + distanceOfRoute d (t : xs)
distanceOfRoute _ _            = 0
