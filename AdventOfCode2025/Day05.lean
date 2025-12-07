import Mathlib.Data.List.MinMax
import Std.Data.HashSet

namespace Day05


private def readInputFile (fileSuffix : String) : IO (List (Nat × Nat) × List Nat) := do
  let fileName := s!"data/day_05_{fileSuffix}.txt"
  let contents ← IO.FS.readFile fileName
  let split := contents.splitOn "\n\n"
  let header := split[0]!
  let footer := split[1]!
  let ranges := header.splitOn "\n"
  |> List.map (fun val => val.splitOn "-")
  |> List.map (fun val =>
    let lower := val[0]!.toNat!
    let upper := val[1]!.toNat!
    (lower, upper)
    )
  let ingredients := footer.splitOn "\n"
  |> List.map (fun val => val.toNat!)
  return (ranges, ingredients)

def withinRanges (ranges: List (Nat × Nat)) (ingredient : Nat) : Bool :=
  ranges.any (fun range =>
    let (lower, upper) := range
    ingredient >= lower && ingredient <= upper
    )

def findFreshIngredients (ranges: List (Nat × Nat)) (ingredients : List (Nat)): List (Nat) :=
  ingredients.filter (withinRanges ranges)

def partA (fileSuffix : String) : IO Unit := do
  IO.println "Running Day 5, Part A"

  let input ← readInputFile fileSuffix
  let (ranges, ingredients) := input

  let freshIngredients := findFreshIngredients ranges ingredients

  let result := freshIngredients.length
  IO.println s!"result:   {result}"

  if fileSuffix.toSlice.contains "sample" then
    let contents ← IO.FS.readFile s!"data/day_05_{fileSuffix}_expected_a.txt"
    IO.println s!"expected: {contents}"

def sumRanges (ranges : List (Nat × Nat)) : Int :=
  (ranges.map (fun range =>
      let (lower, upper) := range
      upper - lower
      ))
    |> List.foldl (· + ·) 0

def findFirstIntersect (ranges : Std.HashSet (Nat × Nat)) : Option ( (Nat × Nat) × (Nat × Nat) ) :=
  none -- TODO

def split (a : (Nat × Nat)) (b : (Nat × Nat)) : ((Nat × Nat) × (Nat × Nat) × (Nat × Nat)) :=
  let (a_1 , a_2) := a
  let (b_1 , b_2) := b

  let ((lower_1, lower_2), (upper_1, upper_2)) :=
    if a_1 < b_1 then ((a_1,a_2), (b_1, b_2))
    else if b_1 < a_1 then ((b_1, b_2), (a_1, a_2))
    else if a_2 < b_2 then ((a_1,a_2), (b_1, b_2))
    else if b_2 < a_2 then ((b_1, b_2), (a_1, a_2))
    else ((a_1,a_2), (b_1, b_2))

  -- case 1 lower == upper
  --   _________________
  --  /                 \
  -- |                  |
  -- .                  .
  -- .                  .
  -- |                  |
  --  ₐ________________/
  if a == b then
    (a, a, a)
  -- case 2 lower_1 == upper_1 && lower_2 < upper_2
  --   _________
  --  /         \
  -- |          |
  -- .          .
  -- .                  .
  -- |                  |
  --  ₐ________________/
  else if lower_1 == upper_1 && lower_2 < upper_2 then
    -- okay to repeat, hashset will take care of it
    ((lower_1,lower_2), (lower_2+1, upper_2), (lower_2+1, upper_2))
  -- case 3 lower_1 < upper_1 && lower_2 == upper_2
  --           _________
  --          /         \
  --         |          |
  --         .          .
  -- .                  .
  -- |                  |
  --  ₐ________________/
  else if lower_1 < upper_1 && lower_2 == upper_2 then
    -- okay to repeat, hashset will take care of it
    ((lower_1, upper_1), (upper_1+1, upper_2), (upper_1+1, upper_2))
  -- case 3 lower_1 < upper_1 && lower_2 > upper_2
  --       _________
  --      /         \
  --     |          |
  --     .          .
  -- .                  .
  -- |                  |
  --  ₐ________________/
  else if lower_1 < upper_1 && lower_2 == upper_2 then
    -- okay to repeat, hashset will take care of it
    ((lower_1, upper_1), (upper_1+1, upper_2), (upper_1+1, upper_2))
  else
    (a, a, b)


def splitFirst (ranges : Std.HashSet (Nat × Nat)) : Std.HashSet (Nat × Nat) :=
  match findFirstIntersect ranges with
  | none => ranges
  | some (a, b) =>
    let (left, middle, right) := split a b
    ranges.erase a
    |>.erase b
    |>.insert left
    |>.insert middle
    |>.insert right

partial def splitIntersectingRecurse (ranges : Std.HashSet (Nat × Nat)) (acc : Std.HashSet (Nat × Nat)): Std.HashSet (Nat × Nat) :=
  if ranges.size == acc.size then
    acc
  else
    splitIntersectingRecurse acc (splitFirst acc)

def splitIntersecting (ranges : List (Nat × Nat)) : Std.HashSet (Nat × Nat) :=
  let hashedRanges := (Std.HashSet.ofList ranges)
  splitIntersectingRecurse hashedRanges (splitFirst hashedRanges)

def partB (fileSuffix : String) : IO Unit := do
  IO.println "Running Day 5, Part B"

  let (ranges, _) ← readInputFile fileSuffix

  let result := sumRanges (splitIntersecting ranges).toList
  IO.println s!"result:   {result}"

  if fileSuffix.toSlice.contains "sample" then
    let contents ← IO.FS.readFile s!"data/day_05_{fileSuffix}_expected_b.txt"
    IO.println s!"expected: {contents}"

def estimateProblemSize : IO Unit := do
  let input <- readInputFile "real"
  let (ranges, _) := input
  let sum :=  sumRanges ranges
  IO.println s!"sum={sum}"
  -- 424424047587505
  -- too big to use a set with every number so we need to split up intersecting ranges

#eval! do
  partA "sample"
  partA "real"
  estimateProblemSize
  partB "sample"
  partB "real"
