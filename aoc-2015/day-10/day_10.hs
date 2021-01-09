-- stack --resolver lts-16.27 script --optimize

main :: IO ()
main = do
  content <- readFile "input.txt"
  putStrLn $ "Part 1: " ++ (show . length) ((iterate lookAndSay content) !! 40)
  putStrLn $ "Part 2: " ++ (show . length) ((iterate lookAndSay content) !! 50)


lookAndSay :: String -> String
lookAndSay (s : rest) = ls 1 s rest
 where
  ls :: Int -> Char -> String -> String
  ls n c (x : xs) | c == x    = ls (n + 1) c xs
                  | otherwise = (show n) ++ (c : []) ++ (ls 1 x xs)
  ls n c [] = (show n) ++ (c : [])
lookAndSay [] = error ("Not a valid string.")
