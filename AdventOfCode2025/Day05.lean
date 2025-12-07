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

def distjoint (a : (Nat × Nat))  (b : (Nat × Nat)) : Bool :=
  let (a_1 , a_2) := a
  let (b_1 , b_2) := b

  a_2 < b_1 || b_2 < a_1

def intersect (a : (Nat × Nat))  (b : (Nat × Nat)) : Bool :=
  ! (distjoint a b)

def findFirstInsertsectWith (val : (Nat × Nat)) (ranges : List (Nat × Nat)) : Option (Nat × Nat) :=
  ranges.find? (intersect val)

def findFirstIntersectRecurse (ranges : List (Nat × Nat)) (acc : Option ( (Nat × Nat) × (Nat × Nat) )) : Option ( (Nat × Nat) × (Nat × Nat) ) :=
  match acc, ranges with
  | some val, _ => val
  | none, [] => none
  | none, head :: rest =>
    let firstIntersectWith := (findFirstInsertsectWith head rest)
    let newAcc := firstIntersectWith.map ( fun val => (head, val) )
    findFirstIntersectRecurse
      rest
      newAcc

def findFirstIntersect (ranges : Std.HashSet (Nat × Nat)) : Option ( (Nat × Nat) × (Nat × Nat) ) :=
  findFirstIntersectRecurse ranges.toList none

def combine (a : (Nat × Nat)) (b : (Nat × Nat)) : (Nat × Nat) :=
  let (a_1 , a_2) := a
  let (b_1 , b_2) := b

  let lower :=
    if a_1 < b_1 then a_1
    else b_1
  let upper :=
    if a_2 > b_2 then a_2
    else b_2

  (lower, upper)

def combineFirst (ranges : Std.HashSet (Nat × Nat)) : Std.HashSet (Nat × Nat) :=
  match findFirstIntersect ranges with
  | none => ranges
  | some (a, b) =>
    ranges.erase a
    |>.erase b
    |>.insert (combine a b)

partial def combineIntersectingRecurse (ranges : Std.HashSet (Nat × Nat)) (acc : Std.HashSet (Nat × Nat)): Std.HashSet (Nat × Nat) :=
  if ranges.size == acc.size then
    acc
  else
    combineIntersectingRecurse acc (combineFirst acc)

def combineIntersecting (ranges : List (Nat × Nat)) : Std.HashSet (Nat × Nat) :=
  let hashedRanges := (Std.HashSet.ofList ranges)
  combineIntersectingRecurse hashedRanges (combineFirst hashedRanges)

def partB (fileSuffix : String) : IO Unit := do
  IO.println "Running Day 5, Part B"

  let (ranges, _) ← readInputFile fileSuffix

  let result := sumRanges (combineIntersecting ranges).toList
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
