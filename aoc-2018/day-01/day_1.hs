import qualified Data.Set as Set

main = do
    content <- readFile "input1.txt"
    let numbers = getNumbers content
    putStr "Part 1: "
    print $ sum numbers
    putStr "Part 2: "
    print $ part2 numbers


-- Extract all the numbers from the input file text.
-- Standard read doesn't handle leading + signs.
getNumbers :: String -> [Int]
getNumbers content = map (read::String->Int) $ map (filter notPlusSign) $ lines content
    where
        notPlusSign x = x /= '+'

part2 :: [Int] -> Int
part2 numbers = solve numbers 0 (Set.fromList [0])

-- Find the first freq that is seen twice.
solve :: [Int] -> Int -> Set.Set Int -> Int
solve (x:xs) freq seen
    | Set.member nextTerm seen = nextTerm
    | otherwise = solve (xs ++ [x]) nextTerm $ Set.insert nextTerm seen
    where 
        nextTerm = freq + x
