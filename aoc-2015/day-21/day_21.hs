-- stack --resolver lts-16.27 script --optimize --package split

-- Inspired strongly (after solving idependently) by this post:
-- https://www.reddit.com/r/adventofcode/comments/3xnyoi/day_21_solutions/cy6dp7u

import           Data.List                      ( sortOn
                                                , subsequences
                                                )
import           Data.List.Split                ( splitOn )
import           Text.Printf                    ( printf )

type Player = (Int, (Int, Int))
type Item = Player

weapons :: [Item]
armor :: [Item]
rings :: [Item]
weapons = zip [8, 10, 25, 40, 74] $ zip [4 ..] (repeat 0)
armor = zip [13, 31, 53, 75, 102] $ zip (repeat 0) [1 ..]
rings =
  zip [25, 50, 100, 20, 40, 80] $ zip [1, 2, 3, 0, 0, 0] [0, 0, 0, 1, 2, 3]

loadouts :: [[Item]]
loadouts =
  [ [w, a] ++ rs
  | w  <- weapons
  , a  <- (0, (0, 0)) : armor
  , rs <- filter ((<= 2) . length) $ subsequences rings
  ]

hits :: Int -> Int -> Int
hits health damage = health `quot` (max 1 damage)

readBoss :: IO Player
readBoss = fmap parse (readFile "input.txt")
 where
  parse s = (hp, (dp, ar))
    where [hp, dp, ar] = map (read . last . splitOn ": ") $ lines s

main :: IO ()
main = do
  (bossHP, (bossDmg, bossArm)) <- readBoss
  let cost = sum . map fst
  let dmg  = sum . map (fst . snd)
  let arm  = sum . map (snd . snd)
  let viable items =
        hits bossHP (dmg items - bossArm) <= hits 100 (bossDmg - arm items)
  let l = sortOn cost loadouts

  printf "Part 1: %d\n" $ head . map cost . filter viable $ l
  printf "Part 2: %d\n" $ head . map cost . filter (not . viable) $ reverse l
