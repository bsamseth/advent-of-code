-- stack --resolver lts-16.27 script --optimize --package split

import           Data.List.Split                ( splitOn )

data Ingredient = Ingredient
  { name       :: String
  , capacity   :: Int
  , durability :: Int
  , flavor     :: Int
  , texture    :: Int
  , calories   :: Int
  }
data Dose = Dose Int Ingredient

main :: IO ()
main = do
  content <- readFile "input.txt"
  let ingredients = map parseIngredient $ lines content
  putStrLn $ "Part 1: " ++ show (maximum $ map score $ combinations ingredients)
  putStrLn $ "Part 2: " ++ show
    (maximum $ map score $ filter has500Cals $ combinations ingredients)

parseIngredient :: String -> Ingredient
parseIngredient line = Ingredient name_ cap dur fla tex cal
 where
  (name_ : definition : []) = splitOn ": " line
  (cap : dur : fla : tex : cal : []) =
    map (read . head . tail) $ map words $ splitOn ", " definition

combinations :: [Ingredient] -> [[Dose]]
combinations ingredients = allocate ingredients 0
 where
  allocate (x : []) used = [[(Dose (100 - used) x)]]
  allocate (x : xs) used =
    [ (Dose n x : sub)
    | n   <- [0 .. (100 - used)]
    , sub <- allocate xs (used + n)
    ]
  allocate [] _ = []

score :: [Dose] -> Int
score doses = foldl
  (*)
  1
  [ max 0 $ sum $ map (f field) doses
  | field <- [capacity, durability, flavor, texture]
  ]
  where f field (Dose n ing) = field ing * n

has500Cals :: [Dose] -> Bool
has500Cals doses = (sum . map f) doses == 500
  where f (Dose n ing) = calories ing * n
