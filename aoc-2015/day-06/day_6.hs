-- stack --resolver lts-16.27 script --optimize

main :: IO ()
main = do
  content <- readFile "input.txt"
  putStrLn $ "Part 1: "
  putStrLn $ "Part 2: "
