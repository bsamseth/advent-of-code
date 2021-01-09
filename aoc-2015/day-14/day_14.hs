-- stack --resolver lts-16.27 script --optimize --package split,containers

import           Data.List.Split                ( splitOn )
import qualified Data.Map                      as M

data Raindeer = Raindeer
  { name     :: String
  , speed    :: Int
  , flyTime  :: Int
  , restTime :: Int
  }
  deriving (Show, Eq, Ord)

main :: IO ()
main = do
  content <- readFile "input.txt"
  let raindeer = map parseLine $ lines content
  putStrLn $ "Part 1: " ++ show
    (maximum $ map (calculateLocation 2503) raindeer)

  putStrLn $ "Part 2: " ++ show (race raindeer)

parseLine :: String -> Raindeer
parseLine line = Raindeer n s f r
 where
  split = splitOn " " line
  n     = head split
  s     = read $ split !! 3
  f     = read $ split !! 6
  r     = read $ split !! 13

calculateLocation :: Int -> Raindeer -> Int
calculateLocation t r =
  speed r
    * flyTime r
    * fullCycles
    + speed r
    * (min (flyTime r) (t `mod` cycleTime))
 where
  cycleTime  = flyTime r + restTime r
  fullCycles = t `div` cycleTime

race :: [Raindeer] -> Int
race raindeer = f raindeer (M.fromList [ (r, 0) | r <- raindeer ]) 1
 where
  f _  scores 2504 = maximum $ map snd $ M.toList scores
  f rs scores t    = f rs newScores (t + 1)
   where
    leadingDist = maximum $ map (calculateLocation t) rs
    newScores =
      M.fromList
        $ [ ( r
            , (scores M.! r) + fromEnum ((calculateLocation t r) == leadingDist)
            )
          | r <- rs
          ]
