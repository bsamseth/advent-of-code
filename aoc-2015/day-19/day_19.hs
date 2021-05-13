-- stack --resolver lts-16.27 script --optimize --package split,containers

import           Data.Char                      ( isLower )
import           Data.List                      ( isPrefixOf
                                                , sortOn
                                                )
import           Data.List.Split                ( splitOn )
import           Data.Ord                       ( comparing )

import qualified Data.Set                      as S
import           Text.Printf                    ( printf )

main :: IO ()
main = do
  content <- readFile "input.txt"
  let (r : t : []) = splitOn "\n\n" content
  let repls        = map parseReplacement $ lines r
  let sortedRepls  = reverse $ sortOn (length . repTo) repls
  let target       = (splitMolecule . head . splitOn "\n") t

  printf "Part 1: %d\n" (S.size (moleculesFrom [] target repls))
  printf "Part 2: %d\n" (countReplacements target)

-- | Replacement from some molecule to something else.
data R = R
  { repFrom :: String
  , repTo   :: String
  }
  deriving Show


parseReplacement :: String -> R
parseReplacement s = R f t where (f : t : []) = splitOn " => " s

splitMolecule :: String -> [String]
splitMolecule [] = []
splitMolecule (x : xs) =
  (x : takeWhile isLower xs) : (splitMolecule (dropWhile isLower xs))


moleculesFrom :: [String] -> [String] -> [R] -> S.Set String
moleculesFrom _    []       _     = S.empty
moleculesFrom past (x : xs) repls = S.union
  (S.fromList [ concat (past ++ [r] ++ xs) | r <- rs ])
  (moleculesFrom (past ++ [x]) xs repls)
  where rs = [ t | (R f t) <- repls, f == x ]

-- Numerous attempts at searches (dfs, bfs, A*, greedy) tried.
-- Greedy doesn't converge for this input, and the others take forever.
-- Finally stole the solution after reading about it. Apparently some people
-- got lucky with their inputs and it worked with a search. This is the general
-- solution.
--
-- Insight 1: Do the search from the formula to e, rather than the other way.
-- This bit was pretty clear and I had this for all the searches anyway.
--
-- Insight: Replace `Rn` with `(`, `Y` with `,` and `Ar` with `)`. Then all
-- replacements are on this form
--
--   A => BC | B(C) | B(C,D) | B(C,D,E)
--
-- where A, B, C, D and E are elements (H, or Ca, or Mg etc.) and may or may not
-- be unique.
--
-- If all where of the first form, it would take N - 1 replacements, where N is the
-- number of elements in the target molecule (still counting `(`, `)` and `,` as single elements).
-- The `B(C)` pattern is still reduced in a single step, meaning the `(` and the `)` are removed
-- without any additional cost, and can therefore be subtracted from N. Going up to
-- `B(C,D)` and `B(C,D,E)` we see that for each comma added we save two elements, i.e.
-- the `,D` and `,D,E`. The final formula is then
--
--  N - 1 - (# of `(` or `)`) - 2 * (# of `,`)
countReplacements :: [String] -> Int
countReplacements []          = -1
countReplacements ("Rn" : xs) = countReplacements xs
countReplacements ("Ar" : xs) = countReplacements xs
countReplacements ("Y"  : xs) = (countReplacements xs) - 1
countReplacements (_    : xs) = (countReplacements xs) + 1
