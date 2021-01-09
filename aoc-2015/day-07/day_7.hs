-- stack --resolver lts-16.27 script --optimize --package split,containers 

import qualified Data.Bits                     as Bits
import           Data.Char                      ( isDigit )
import           Data.List.Split                ( splitOn )
import qualified Data.Map                      as Map
import           Data.Word                      ( Word16 )

main :: IO ()
main = do
  content <- readFile "input.txt"
  let instructions = lines content
  let assignments = Map.fromList $ map parseInstruction instructions
  let wires        = evaluate (Wire "a") assignments Map.empty
  let signalA      = wires Map.! (Wire "a")
  putStrLn $ "Part 1: " ++ show signalA

  let updatedAssignments = Map.insert
        (Wire "b")
        (Assignment (Wire "b") (NOP (Wire (show signalA))))
        assignments
  let updatedWires   = evaluate (Wire "a") updatedAssignments Map.empty
  let updatedSignalA = updatedWires Map.! (Wire "a")
  putStrLn $ "Part 2: " ++ show updatedSignalA


-- | A wire is a string holding either a label (e.g. "a") or a direct number (e.g. "123")
data Wire = Wire String
  deriving (Show, Eq, Ord)
-- | An Op represents an operation on one or two Wires, with the binary operation being one of NOP, AND, OR, NOT, LSHIFT, RSHIFT.
data Op = NOP Wire | AND Wire Wire | OR Wire Wire | NOT Wire | LSHIFT Wire Wire | RSHIFT Wire Wire deriving (Show, Eq)
-- | An assignment models the relationship `wire x <- result of operation y`
data Assignment = Assignment Wire Op
  deriving (Show, Eq)

fromWire :: Wire -> String
fromWire (Wire x) = x


-- | Produce pairs of (Wire x, Assignement for Wire x), to be used with Map.fromList.
-- Takes a line from the input file as its argument.
parseInstruction :: String -> (Wire, Assignment)
parseInstruction s = (Wire t, Assignment (Wire t) op)
 where
  (f : t : []) = splitOn " -> " s
  split        = splitOn " " f
  op           = case (length split) of
    1 -> NOP (Wire f)
    2 -> NOT (Wire (split !! 1))
    3 -> binOp (Wire lhs) (Wire rhs)
     where
      (lhs : opName : rhs : []) = split
      binOp                     = case (opName) of
        "AND"    -> AND
        "OR"     -> OR
        "LSHIFT" -> LSHIFT
        "RSHIFT" -> RSHIFT
        _        -> error ("Unexpected operation name")
    _ -> error ("Unexpected line length" ++ (show (length split)))


-- | Return a map from Wire to Word16 with the given wire being guaranteed to be present.
-- Any other wires needed to compute the given wire will also be in the returned map.
evaluate
  :: Wire
  -> Map.Map Wire Assignment
  -> Map.Map Wire Word16
  -> Map.Map Wire Word16
evaluate w assignments cache
  | Map.member w cache       = cache
  |  -- w already in the cache, nothing needed.
    all isDigit (fromWire w) = Map.insert w (read (fromWire w)) cache
  |  -- w holds a direct number, nothing to compute, just insert.
    otherwise                = insertNew $ compute (assignments Map.! w)  -- Find the assignment, compute the related value and update the cache.
 where
  insertNew (c, v) = Map.insert w v c  -- Populate the cache c with the computed value v for Wire w.

  -- The compute function takes an assignment and produced an (possibly) updaed cache and a compute value for Wire w.
  -- This is done by first evaluating any requried operands, followed by performing the required binary operation.
  compute (Assignment (Wire _) (NOP ww)) = (c, c Map.! ww)
    where c = evaluate ww assignments cache
  compute (Assignment (Wire _) (AND x y)) =
    (c, (c Map.! x) Bits..&. (c Map.! y))
   where
    c_ = evaluate x assignments cache
    c  = evaluate y assignments c_
  compute (Assignment (Wire _) (OR x y)) =
    (c, (c Map.! x) Bits..|. (c Map.! y))
   where
    c_ = evaluate x assignments cache
    c  = evaluate y assignments c_
  compute (Assignment (Wire _) (LSHIFT x y)) =
    (c, Bits.shift (c Map.! x) (fromIntegral (c Map.! y) :: Int))
   where
    c_ = evaluate x assignments cache
    c  = evaluate y assignments c_
  compute (Assignment (Wire _) (RSHIFT x y)) =
    (c, Bits.shift (c Map.! x) (negate (fromIntegral (c Map.! y) :: Int)))
   where
    c_ = evaluate x assignments cache
    c  = evaluate y assignments c_
  compute (Assignment (Wire _) (NOT x)) = (c, Bits.complement $ c Map.! x)
    where c = evaluate x assignments cache
