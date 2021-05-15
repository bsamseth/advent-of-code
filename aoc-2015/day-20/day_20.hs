-- stack --resolver lts-16.27 script --optimize

-- This solution is largely stolen from a solution posted in the Megathread.
-- Solved in C++ mainly, this is much slower but really nice, so I kept it for
-- reference. See original:
-- https://www.reddit.com/r/adventofcode/comments/3xjpp2/day_20_solutions/cy5ou98

import           Data.List                      ( findIndex
                                                , nub
                                                )
import           Data.Maybe                     ( fromJust )
import           Text.Printf                    ( printf )

main :: IO ()
main = do
  content <- readFile "input.txt"
  let target = read content :: Int
  printf "Part 1: %d\n" $ findFirstHouse target presents
  printf "Part 2: %d\n" $ findFirstHouse target presents2

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

factors :: Int -> [Int]
factors n =
  nub
    . concat
    $ [ [p, q] | p <- [1 .. isqrt (n)], let (q, r) = divMod n p, r == 0 ]

presents :: Int -> Int
presents = (10 *) . sum . factors

presents2 :: Int -> Int
presents2 n = (11 *) . sum . filter (> (n - 1) `div` 50) . factors $ n

findFirstHouse :: Int -> (Int -> Int) -> Int
findFirstHouse target presentCounter =
  fromJust . findIndex (>= target) . map presentCounter $ [0 ..]

