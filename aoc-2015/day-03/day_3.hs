-- stack --resolver lts-16.27 script --package containers --optimize

import qualified Data.Set as Set

main :: IO ()
main = do
  content <- readFile "input.txt"
  let directions = map charToDir content
  putStrLn $ "Part 1: " ++ show (Set.size $ allLocations directions)
  putStrLn $ "Part 2: " ++ show (Set.size (Set.union (allLocations (first directions)) (allLocations (second directions))))

data Dir = Dir Int Int deriving (Show, Eq, Ord)

charToDir :: Char -> Dir
charToDir c
  | c == '>' = Dir 1 0
  | c == '<' = Dir (-1) 0
  | c == 'v' = Dir 0 (-1)
  | c == '^' = Dir 0 1
  

locations :: Dir -> [Dir] -> Set.Set Dir -> Set.Set Dir
locations p [] s = Set.insert p s
locations (Dir x y) (Dir dx dy:xs) s =
  Set.insert (Dir x y) $
    locations (Dir (x+dx) (y+dy)) xs s

allLocations :: [Dir] -> Set.Set Dir
allLocations dirs = locations (Dir 0 0) dirs (Set.fromList [])

first :: [a] -> [a]
first (x:y:xs) = x : first xs;
first _ = []

second :: [a] -> [a]
second (x:y:xs) = y : second xs;
second _ = []
