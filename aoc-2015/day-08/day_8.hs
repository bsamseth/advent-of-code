-- stack --resolver lts-16.27 script --optimize

main :: IO ()
main = do
  content <- readFile "input.txt"
  let strings = lines content
  putStrLn $ "Part 1: " ++ (show . sum) (map (compareLengths sourceCodeLength memoryLength) strings)
  putStrLn $ "Part 2: " ++ (show . sum) (map (compareLengths encodedSourceCodeLength sourceCodeLength) strings)

compareLengths :: (String -> Int) -> (String -> Int) -> String -> Int
compareLengths x y s = (x s) - (y s)

sourceCodeLength :: String -> Int
sourceCodeLength = length

memoryLength :: String -> Int
memoryLength s = memlen s - 2
  where
    memlen ('\\':'x':_:_:xs) = 1 + memlen (xs)
    memlen ('\\':_:xs) = 1 + memlen (xs)
    memlen (_:xs) = 1 + memlen (xs)
    memlen [] = 0

encodedSourceCodeLength :: String -> Int
encodedSourceCodeLength s = smemlen s + 2
  where
    smemlen ('"':xs) = 2 + smemlen xs
    smemlen ('\\':xs) = 2 + smemlen xs
    smemlen (_:xs) = 1 + smemlen xs
    smemlen [] = 0

