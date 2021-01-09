-- stack --resolver lts-16.27 script --package split --optimize

import           Data.List                      ( sort )
import           Data.List.Split                ( splitOn )

main :: IO ()
main = do
  content <- readFile "input.txt"
  let orders = lines content
  putStrLn $ "Part 1: " ++ (show . sum . map requriedWrappingPaper) orders
  putStrLn $ "Part 1: " ++ (show . sum . map requiredRibbon) orders

dimensions :: String -> [Int]
dimensions order = map read $ splitOn "x" order

requriedWrappingPaper :: String -> Int
requriedWrappingPaper order = 2 * (a + b + c) + minimum [a, b, c]
 where
  a                = w * h
  b                = w * l
  c                = h * l
  (w : l : h : []) = dimensions order

requiredRibbon :: String -> Int
requiredRibbon order = 2 * a + 2 * b + a * b * c
  where (a : b : c : []) = sort $ dimensions order

