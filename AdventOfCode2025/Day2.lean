import Mathlib.Data.List.MinMax

namespace Day2


private def parseLine (token : String) : Option (Int × Int) := do
  -- Get first character (direction)
  match token.splitOn "-" with
  | [a, b] => do
    let aNat <- a.toNat?
    let bNat <- b.toNat?
    return (aNat, bNat)
  | _ => none

private def readInputFile (fileSuffix : String) : IO (List (Int × Int)) := do
  let fileName := s!"data/day_02_{fileSuffix}.txt"
  let contents ← IO.FS.readFile fileName
  let lines := contents.splitOn ","
  let lines := lines.filter (fun line => line.trim != "")

  -- Parse each line into a tuple
  let parsed := lines.filterMap parseLine

  return parsed

private def concatNumbers (a b : Int) : Int :=
  let s := toString a ++ toString b
  s.toNat!

private partial def generateErrorsRecurse (current : Int) (exitCondition : Int) (acc : List Int) : List Int :=
  let concated := concatNumbers current current
  if concated > exitCondition then
    acc
  else
    generateErrorsRecurse (current + 1) exitCondition (concated :: acc)

private def generateErrors(exitCondition : Int) : List (Int) :=
  generateErrorsRecurse 1 exitCondition []

def partA (fileSuffix : String) : IO Unit := do
  IO.println "Running Day 2, Part A"
  let ranges ← readInputFile fileSuffix
  let max ← (ranges.map (·.1) ++ ranges.map (·.2)).maximum?
  IO.println s!"{ranges}"

  if fileSuffix.toSlice.contains "sample" then
    let contents ← IO.FS.readFile s!"data/day_02_{fileSuffix}_expected.txt"
    IO.println s!"expected: ({contents})"


def partB (fileSuffix : String) : IO Unit := do
  IO.println "Running Day 2, Part B"

  if fileSuffix.toSlice.contains "sample" then
    let contents ← IO.FS.readFile s!"data/day_02_{fileSuffix}_expected.txt"
    IO.println s!"expected: ({contents})"

#eval
  partA "sample"
