-- stack --resolver lts-16.27 script --optimize

import Data.Char (chr, ord)

main :: IO ()
main = do
  password <- readFile "input.txt"
  let (p1:p2:[]) = take 2 $ nextValidPasswords password
  putStrLn $ "Part 1: " ++ p1
  putStrLn $ "Part 2: " ++ p2
  
nextChar :: Char -> Char
nextChar c = case c of
  'h' -> 'j'  -- Skip i
  'k' -> 'm'  -- Skip l
  'n' -> 'p'  -- Skip o
  _ -> chr $ ord c + 1

hasStraight :: String -> Bool
hasStraight (a:b:c:xs)
  | (1, 2) == (ord b - ord a, ord c - ord a) = True
  | otherwise = hasStraight (b:c:xs)
hasStraight _ = False

twoPairs :: String -> Bool
twoPairs s = f s False
  where
    f (a:b:xs) p
      | a == b && p = True
      | a == b = f xs True
      | otherwise = f (b:xs) p
    f _ _ = False

nextPassword :: String -> String
nextPassword s = reverse $ np $ reverse s
  where
    np :: String -> String
    np (a:b:xs)
      | a == 'z' = 'a':(np (b:xs))     -- wrap around and recurse on next character.
      | otherwise = (nextChar a):b:xs  -- Iterate char and end recursion.
    np xs = map nextChar xs

nextValidPasswords :: String -> [String]
nextValidPasswords s = filter valid $ iterate nextPassword s
  where
    valid p = hasStraight p && twoPairs p
