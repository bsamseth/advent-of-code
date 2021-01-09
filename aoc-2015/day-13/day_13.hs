-- stack --resolver lts-16.27 script --optimize --package split,containers

import           Data.List                      ( permutations )
import           Data.List.Split                ( splitOn )
import qualified Data.Map                      as M
import qualified Data.Set                      as S

data Pair = Pair
  { left  :: String
  , right :: String
  }
  deriving (Show, Eq, Ord)

main :: IO ()
main = do
  content <- readFile "input.txt"
  let gains  = M.fromList $ map parseLine $ lines content
  let people = S.toList $ S.fromList $ map left $ map fst $ M.toList gains
  putStrLn
    $  "Part 1: "
    ++ show
         (maximum
           [ calculateHappiness order gains | order <- permutations people ]
         )

  let gainsWithMe = M.unions
        [ gains
        , M.fromList [ (Pair "Me" p, 0) | p <- people ]
        , M.fromList [ (Pair p "Me", 0) | p <- people ]
        ]
  putStrLn $ "Part 2: " ++ show
    (maximum
      [ calculateHappiness order gainsWithMe
      | order <- permutations ("Me" : people)
      ]
    )

parseLine :: String -> (Pair, Int)
parseLine s = (Pair l r, gain)
 where
  (l : _ : pm : hp : _ : _ : _ : _ : _ : _ : r : []) =
    splitOn " " $ head $ splitOn "." s
  happiness = read hp :: Int
  gain | pm == "gain" = happiness
       | otherwise    = -happiness

calculateHappiness :: [String] -> M.Map Pair Int -> Int
calculateHappiness lst g = f (lst ++ [head lst]) g
 where
  f (x : y : xs) gains =
    (gains M.! (Pair y x)) + (gains M.! (Pair x y)) + f (y : xs) gains
  f _ _ = 0
