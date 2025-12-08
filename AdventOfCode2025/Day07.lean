import Mathlib.Data.List.MinMax
import Std.Data.HashSet

namespace Day07


private def readInputFile (fileSuffix : String) : IO (String) := do
  let fileName := s!"data/day_07_{fileSuffix}.txt"
  let contents ← IO.FS.readFile fileName
  return contents

def partA (fileSuffix : String) : IO Unit := do
  IO.println "Running Day 7, Part A"

  let input ← readInputFile fileSuffix
  IO.println s!"input:\n{input}"

  let result := 0
  IO.println s!"result:   {result}"

  if fileSuffix.toSlice.contains "sample" then
    let contents ← IO.FS.readFile s!"data/day_07_{fileSuffix}_expected_a.txt"
    IO.println s!"expected: {contents}"


def partB (fileSuffix : String) : IO Unit := do
  IO.println "Running Day 7, Part B"

  let input ← readInputFile fileSuffix

  let result := 0
  IO.println s!"result:   {result}"

  if fileSuffix.toSlice.contains "sample" then
    let contents ← IO.FS.readFile s!"data/day_07_{fileSuffix}_expected_b.txt"
    IO.println s!"expected: {contents}"


#eval! do
  partA "sample"
  partA "real"
