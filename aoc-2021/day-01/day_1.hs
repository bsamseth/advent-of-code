-- stack --resolver lts-16.27 script 

main :: IO ()
main = do
  content <- readFile "input.txt"
  let numbers = map read $ lines content
  putStrLn $ "Part 1: " ++ show (countIncreasing numbers)
  putStrLn $ "Part 2: " ++ show (countIncreasing $ movingSum numbers)

movingSum :: [Int] -> [Int]
movingSum (x:[]) = [x]
movingSum x = f : r
  where f = sum $ take 3 x
        r = movingSum $ tail x

countIncreasing :: [Int] -> Int
countIncreasing x = sum $ map comp $ zip x (tail x) 
  where comp (x,y) 
          | x < y = 1
          | otherwise = 0
