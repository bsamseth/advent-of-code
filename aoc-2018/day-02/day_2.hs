import qualified Data.Set as Set
import Data.List (splitAt)
import Data.Maybe (fromMaybe, isNothing)

main = do
    content <- readFile "input1.txt"
    let ids = lines content
    putStr "Part 1: "
    print $ part1 ids
    putStrLn $ "Part 2: " ++ (fromMaybe "" (part2 ids))

-- ======  Part 1 =====
part1 :: [String] -> Int
part1 ids = a * b
    where 
        (a, b) = pairSum $ map twoAndThreeCount ids

-- Compute the columnwise sum.
pairSum :: [(Int, Int)] -> (Int, Int)
pairSum [] = (0, 0)
pairSum ((a, b):xs) = (a + fst rest, b + snd rest)
    where rest = pairSum xs

twoAndThreeCount :: String -> (Int, Int)
twoAndThreeCount id = (hasTwo, hasThree)
    where charCounts = map (charCount id) id
          hasTwo = if any (== 2) charCounts then 1 else 0
          hasThree = if any (== 3) charCounts then 1 else 0

charCount :: String -> Char -> Int
charCount str char = length $ filter (== char) str


-- ======  Part 2 =====
part2 :: [String] -> Maybe String 
part2 ids = head $ dropWhile isNothing maybeDuplicates
    where 
        maybeDuplicates = [
            extractDuplicate (map (dropNthElement n) ids) 
                             (Set.fromList []) 
            | n <- [0..]]

extractDuplicate :: Ord a => [a] -> Set.Set a -> Maybe a
extractDuplicate [] seen = Nothing
extractDuplicate (id:ids) seen 
    | Set.member id seen = Just id
    | otherwise = extractDuplicate ids $ Set.insert id seen

dropNthElement :: Int -> [a] -> [a]
dropNthElement n lst = first ++ tail rest
    where (first, rest) = splitAt n lst

