-- stack --resolver lts-16.27 script --optimize --package containers

import qualified Data.Map                      as M
import           Text.Printf                    ( printf )

main :: IO ()
main = do
  content <- readFile "input.txt"
  let ways = waysToFill 150 0 $ readUnits content
  printf "Part 1: %d\n" $ M.foldl' (+) 0 ways
  printf "Part 2: %d\n" $ (snd . M.findMin) ways

readUnits :: String -> [Int]
readUnits s = map read $ lines s

waysToFill :: Int -> Int -> [Int] -> M.Map Int Int
waysToFill 0 n_used _  = M.singleton n_used 1
waysToFill _ _      [] = M.empty
waysToFill n n_used (x : xs)
  | n < 0 = M.empty
  | otherwise = M.unionWith (+)
                            (waysToFill (n - x) (n_used + 1) xs)
                            (waysToFill n n_used xs)
