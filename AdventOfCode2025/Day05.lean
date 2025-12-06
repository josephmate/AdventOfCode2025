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
  IO.println s!"input:\n{input}"
  let (ranges, ingredients) := input

  let freshIngredients := findFreshIngredients ranges ingredients

  let result := freshIngredients.length
  IO.println s!"result:   {result}"

  if fileSuffix.toSlice.contains "sample" then
    let contents ← IO.FS.readFile s!"data/day_05_{fileSuffix}_expected_a.txt"
    IO.println s!"expected: {contents}"

def partB (fileSuffix : String) : IO Unit := do
  IO.println "Running Day 5, Part B"

  let input ← readInputFile fileSuffix
  IO.println s!"input:\n{input}"

  let result := 0
  IO.println s!"result:   {result}"

  if fileSuffix.toSlice.contains "sample" then
    let contents ← IO.FS.readFile s!"data/day_05_{fileSuffix}_expected_b.txt"
    IO.println s!"expected: {contents}"

#eval! do
  partA "sample"
  partA "real"
