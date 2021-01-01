module AocHaskell.Input (getInput) where
import Text.Printf (printf)

getInput :: Int -> Int -> IO String
getInput year day = do
  readFile $ "aoc-" ++ (show year) ++ "/day-" ++ (printf "%02d" day) ++ "/input.txt"


