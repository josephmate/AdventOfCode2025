import Mathlib.Data.List.MinMax
import Std.Data.HashSet

namespace Day06


private def readInputFile (fileSuffix : String) : IO (String) := do
  let fileName := s!"data/day_06_{fileSuffix}.txt"
  let contents ← IO.FS.readFile fileName
  return contents

def parseTable (s : String) : List (String × List Nat) :=
  s.splitOn "\n"
  |> List.map (·.splitOn.filter (· ≠ ""))
  |> List.transpose
  |> List.map (fun list =>
      let last := list.getLast!
      let numbers := list.dropLast.map String.toNat!

      (last, numbers)
    )

def calculateHomework (homework : List (String × List Nat)) : List (Nat) :=
  homework.map (fun row =>
    let (op, numbers) := row

    if op == "*" then
      numbers.prod
    else
      numbers.sum
    )

def partA (fileSuffix : String) : IO Unit := do
  IO.println "Running Day 6, Part A"

  let input ← readInputFile fileSuffix
  let table := parseTable input
  IO.println s!"table:\n{table}"

  let results := calculateHomework table
  IO.println s!"results:   {results}"

  let result := results.sum
  IO.println s!"result:   {result}"

  if fileSuffix.toSlice.contains "sample" then
    let contents ← IO.FS.readFile s!"data/day_06_{fileSuffix}_expected_a.txt"
    IO.println s!"expected: {contents}"


def partB (fileSuffix : String) : IO Unit := do
  IO.println "Running Day 6, Part B"

  let input ← readInputFile fileSuffix

  let result := 0
  IO.println s!"result:   {result}"

  if fileSuffix.toSlice.contains "sample" then
    let contents ← IO.FS.readFile s!"data/day_06_{fileSuffix}_expected_b.txt"
    IO.println s!"expected: {contents}"


#eval! do
  partA "sample"
  partA "real"
