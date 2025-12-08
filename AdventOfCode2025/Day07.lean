import Mathlib.Data.List.MinMax
import Std.Data.HashSet

namespace Day07


private def readInputFile (fileSuffix : String) : IO (String) := do
  let fileName := s!"data/day_07_{fileSuffix}.txt"
  let contents ← IO.FS.readFile fileName
  return contents

def parseProblem (mapStr : String) : (Int × List (Std.HashSet (Int))) :=
  let lines := mapStr.splitOn "\n"
  let (header, rest) := match lines with
  | head :: tail => (head, tail)
  | [] => panic! "input cannot be empty"

  let Sidx := (header.find (· == 'S')).byteIdx
  let (_, everyOtherLine) : (Int × List (String)) :=
    rest.foldl (fun acc val =>
      let (idx, list) := acc
      if idx % 2 == 1 then
        (idx+1, val :: list)
      else
        (idx+1, list)
      ) (0, [])

  let splitterPosns : List (Std.HashSet (Int)) := everyOtherLine.reverse.map (fun line =>
    line.toList.zipIdx.filter (fun (c, _) => c == '^')
    |> List.map (fun (_, idx) => idx)
    |> List.foldl (fun acc val => acc.insert val) ∅
  )

  (Sidx, splitterPosns)

def iterateOnce (acc : (Std.HashSet (Int) × Nat)) (splitterLine : Std.HashSet (Int)) : (Std.HashSet (Int) × Nat) :=
  let (beams, count) := acc

  let splittersHit := beams.toList.filter splitterLine.contains
  |> List.length

  let newBeams := (beams.toList.map (fun beamIdx =>
    if splitterLine.contains beamIdx then
      [beamIdx -1, beamIdx + 1]
    else
      [beamIdx]
    )
  )
  |> List.flatten
  |> Std.HashSet.ofList

  (newBeams, count + splittersHit)

def countSplits (Sidx : Int) (splitters : List (Std.HashSet (Int))) : Int :=
  let (_, splitCount) := splitters.foldl iterateOnce (Std.HashSet.ofList [Sidx], 0)

  splitCount

def partA (fileSuffix : String) : IO Unit := do
  IO.println "Running Day 7, Part A"

  let input ← readInputFile fileSuffix
  IO.println s!"input:\n{input}"

  let (Sidx, splitters) := parseProblem input
  IO.println s!"Sidx:\n{Sidx}"
  IO.println s!"splitters:\n{splitters.map Std.HashSet.toList}"

  let result := countSplits Sidx splitters
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
