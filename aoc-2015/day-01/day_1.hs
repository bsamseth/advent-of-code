-- stack --resolver lts-16.27 script 
main :: IO ()
main = do
  content <- readFile "input.txt"
  putStrLn $ "Part 1: " ++ show (part1 content)
  putStrLn $ "Part 2: " ++ show (part2 content)

translated :: String -> [Int]
translated s = map translate s
 where
  translate x | x == '('  = 1
              | otherwise = -1

part1 :: String -> Int
part1 s = (sum . translated) s

part2 :: String -> Int
part2 s = index
 where
  (index, _) = head $ filter in_basement (zip [1 ..] cumsum)
  cumsum     = scanl (+) 0 $ translated s
  in_basement (_, level) = level < 0
