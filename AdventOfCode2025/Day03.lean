import Mathlib.Data.List.MinMax
import Std.Data.HashSet

namespace Day03

private def toDigits (digitsStr : String) : List Int :=
  digitsStr.toList.map (fun digit => digit.toNat - '0'.toNat)

private def readInputFile (fileSuffix : String) : IO (List (List Int)) := do
  let fileName := s!"data/day_03_{fileSuffix}.txt"
  let contents ← IO.FS.readFile fileName
  let lines := contents.splitOn "\n"

  return lines.map toDigits


def maxWithIndexRecurse(list : List Int) (afterIdx : Int) (currentIdx : Int) (acc : Int × Int) : Int × Int :=
  let (maxSoFar, maxSoFarIdx) := acc
  match list with
  | [] => acc
  | head :: tail =>
    let nextAcc :=
      if head > maxSoFar && currentIdx > afterIdx then
        (head, currentIdx)
      else
        acc
    maxWithIndexRecurse tail afterIdx (currentIdx+1) nextAcc

def maxWithIndex(list : List Int) (afterIdx : Int): Int × Int :=
  maxWithIndexRecurse list afterIdx 0 (-1, -1)

def calcBiggest(powerBank : List Int) : Int :=
  let (maxFront, idxFront) := maxWithIndex powerBank.dropLast (-1)
  let (maxOther, _) := maxWithIndex powerBank idxFront

  (toString maxFront ++ toString maxOther).toNat!

def partA (fileSuffix : String) : IO Unit := do
  IO.println "Running Day 3, Part A"

  let powerBanks ← readInputFile fileSuffix
  IO.println s!"powerBanks"
  IO.println s!"{powerBanks}"

  let partialResults := powerBanks.map calcBiggest
  IO.println s!"partialResults"
  IO.println s!"{partialResults}"

  let result := partialResults.foldl (· + ·) 0
  IO.println s!"result:   {result}"

  if fileSuffix.toSlice.contains "sample" then
    let contents ← IO.FS.readFile s!"data/day_03_{fileSuffix}_expected_a.txt"
    IO.println s!"expected: {contents}"


def partB (fileSuffix : String) : IO Unit := do
  IO.println "Running Day 3, Part B"

  if fileSuffix.toSlice.contains "sample" then
    let contents ← IO.FS.readFile s!"data/day_03_{fileSuffix}_expected_b.txt"
    IO.println s!"expected: {contents}"

#eval! do
  partA "sample"
  partA "real"
