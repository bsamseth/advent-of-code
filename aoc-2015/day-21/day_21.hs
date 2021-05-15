-- stack --resolver lts-16.27 script --optimize --package split

import           Data.List                      ( nub
                                                , sort
                                                , sortOn
                                                )
import           Data.List.Split                ( splitOn )
import           Text.Printf                    ( printf )

data CombatStats = CombatStats
  { damage :: Int
  , armor  :: Int
  }
  deriving (Show, Eq, Ord)

data PlayerStats = PlayerStats
  { hitPoints   :: Int
  , combatStats :: CombatStats
  }
  deriving Show

data Item = Item
  { itemName  :: String
  , itemCost  :: Int
  , itemStats :: CombatStats
  }
  deriving (Show, Eq, Ord)

data Category = Category
  { minBuyQuantity :: Int
  , maxBuyQuantity :: Int
  , itemsForSale   :: [Item]
  }
  deriving Show

type Purchase = [Item]
type Store = [Category]


store :: Store
store =
  [ Category
    1
    1
    [ Item "Dagger"     8  (CombatStats 4 0)
    , Item "Shortsword" 10 (CombatStats 5 0)
    , Item "Warhammer"  25 (CombatStats 6 0)
    , Item "Longsword"  40 (CombatStats 7 0)
    , Item "Greataxe"   74 (CombatStats 8 0)
    ]
  , Category
    0
    1
    [ Item "Leather"    13  (CombatStats 0 1)
    , Item "Chainmail"  31  (CombatStats 0 2)
    , Item "Splintmail" 53  (CombatStats 0 3)
    , Item "Bandedmail" 75  (CombatStats 0 4)
    , Item "Platemail"  102 (CombatStats 0 5)
    ]
  , Category
    0
    2
    [ Item "Damage +1"  25  (CombatStats 1 0)
    , Item "Damage +2"  50  (CombatStats 2 0)
    , Item "Damage +3"  100 (CombatStats 3 0)
    , Item "Defense +1" 20  (CombatStats 0 1)
    , Item "Defense +2" 40  (CombatStats 0 2)
    , Item "Defense +3" 80  (CombatStats 0 3)
    ]
  ]


parseStats :: String -> PlayerStats
parseStats s = PlayerStats hp (CombatStats dp ar)
  where [hp, dp, ar] = map (read . (!! 1) . splitOn ": ") (lines s)


combinations :: (Eq a, Ord a) => [a] -> Int -> [[a]]
combinations _  0 = [[]]
combinations [] _ = []
combinations (x : xs) n =
  map (x :) (combinations xs (n - 1)) ++ combinations xs n

purchaseCost :: Purchase -> Int
purchaseCost = sum . map itemCost

possibleCategoryPurchases :: Category -> [Purchase]
possibleCategoryPurchases (Category atLeast atMost items) =
  concat (map (combinations items) [atLeast .. atMost])

possibleStorePurchases :: Store -> [Purchase]
possibleStorePurchases [a, d, r] = foo
 where
  foo =
    [ concat [x, y, z]
    | x <- possibleCategoryPurchases a
    , y <- possibleCategoryPurchases d
    , z <- possibleCategoryPurchases r
    ]
possibleStorePurchases _ = []

applyItems :: PlayerStats -> Purchase -> PlayerStats
applyItems p [] = p
applyItems (PlayerStats hp (CombatStats dp ar)) (Item _ _ (CombatStats d a) : xs)
  = applyItems (PlayerStats hp (CombatStats (dp + d) (ar + a))) xs

fight :: PlayerStats -> PlayerStats -> Bool
fight (PlayerStats hp1 (CombatStats dp1 ar1)) (PlayerStats hp2 (CombatStats dp2 ar2))
  = p1 >= p2
 where
  p1 =
    ceiling $ (/) (fromIntegral hp2 :: Double)
                  (fromIntegral (max 1 (dp1 - ar2)) :: Double) :: Int
  p2 =
    ceiling $ (/) (fromIntegral hp1 :: Double)
                  (fromIntegral (max 1 (dp2 - ar1)) :: Double) :: Int

main :: IO ()
main = do
  content <- readFile "input.txt"
  let boss   = parseStats content
  let player = PlayerStats 100 (CombatStats 0 0)

  -- Sort the possible purchases by price, then apply each set of items to the player.
  -- This way the first fight the player wins is with the least possible cost.
  let ps = sortOn purchaseCost $ possibleStorePurchases store
  let reversePs =
        sortOn ((* (-1)) . purchaseCost) $ possibleStorePurchases store
  printf "Part 1: %d\n"
    $ (purchaseCost . snd . head . filter (fight boss . fst))
    $ zip (map (applyItems player) ps) ps

  printf "Part 2: %d\n"
    $ (purchaseCost . snd . head . filter (not . fight boss . fst))
    $ zip (map (applyItems player) reversePs) reversePs
  print $ length ps
