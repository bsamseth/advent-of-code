main = do
    content <- readFile    "input.txt"
    putStr "Part 1: "
    print $ part1 0 content
    putStr "Part 2: "
    print $ part2 1 0 content

part1 :: Int -> String -> Int
part1 s [] = s
part1 s (x:xs)
  | x == '(' = part1 (s+1) xs
  | otherwise = part1 (s-1) xs

part2 :: Int -> Int -> String -> Int
part2 _ _ [] = -1
part2 i s (x:xs)
    | s + change < 0 = i
    | otherwise = part2 (i+1) (s+change) xs
  where
    change
      | x == '(' = 1
      | otherwise = -1
