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

def between (value : Int) (range : (Int × Int)) : Bool :=
  let (lower, upper) := range
  value >= lower && value <= upper

def rangesContain (ranges : List (Int × Int)) (value : Int) : Bool :=
  ranges.map (between value) |> List.foldl (· || ·) false

def partA (fileSuffix : String) : IO Unit := do
  IO.println "Running Day 2, Part A"
  let ranges ← readInputFile fileSuffix
  let max := (ranges.map (·.1) ++ ranges.map (·.2)).maximum.get!
  IO.println s!"ranges"
  IO.println s!"{ranges}"

  let potentialErrors := generateErrors max
  IO.println s!"potentialErrors"
  IO.println s!"{potentialErrors}"

  let result := potentialErrors.filter (rangesContain ranges) |> List.foldl (· + ·) 0
  IO.println s!"result"
  IO.println s!"{result}"

  if fileSuffix.toSlice.contains "sample" then
    let contents ← IO.FS.readFile s!"data/day_02_{fileSuffix}_expected.txt"
    IO.println s!"expected: ({contents})"

def partB (fileSuffix : String) : IO Unit := do
  IO.println "Running Day 2, Part B"

  if fileSuffix.toSlice.contains "sample" then
    let contents ← IO.FS.readFile s!"data/day_02_{fileSuffix}_expected.txt"
    IO.println s!"expected: ({contents})"

#eval! do
  partA "sample"
  partA "real"
