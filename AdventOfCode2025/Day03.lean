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


def maxWithIndexRecurse(list : List Int) (currentIdx : Int) (acc : Int × Int) : Int × Int :=
  let (maxSoFar, _) := acc
  match list with
  | [] => acc
  | head :: tail =>
    let nextAcc :=
      if head > maxSoFar then
        (head, currentIdx)
      else
        acc
    maxWithIndexRecurse tail (currentIdx+1) nextAcc

def maxWithIndex(list : List Int): Int × Int :=
  maxWithIndexRecurse list 0 (-1, -1)

def calcBiggestNRecurse (N : Nat) (powerBank : List Int)(acc : List Int): List Int :=
match N with
  | 0 => acc
  | n + 1 =>
    let length := powerBank.length
    let (maxFront, idxFront) := maxWithIndex (powerBank.take (length-N+1))
    let subProblem := powerBank.drop (idxFront + 1).toNat?.get!
    calcBiggestNRecurse n subProblem (maxFront :: acc)

def calcBiggestN (N : Nat) (powerBank : List Int): Int :=
  let digits := (calcBiggestNRecurse N powerBank []).reverse

  let digitsStr := String.join (digits.map toString)
  digitsStr.toNat!

def partA (fileSuffix : String) : IO Unit := do
  IO.println "Running Day 3, Part A"

  let powerBanks ← readInputFile fileSuffix

  let partialResults := powerBanks.map (calcBiggestN 2)
  IO.println s!"partialResults"
  IO.println s!"{partialResults}"

  let result := partialResults.foldl (· + ·) 0
  IO.println s!"result:   {result}"

  if fileSuffix.toSlice.contains "sample" then
    let contents ← IO.FS.readFile s!"data/day_03_{fileSuffix}_expected_a.txt"
    IO.println s!"expected: {contents}"



def partB (fileSuffix : String) : IO Unit := do
  IO.println "Running Day 3, Part B"

  let powerBanks ← readInputFile fileSuffix

  let partialResults := powerBanks.map (calcBiggestN 12)
  IO.println s!"partialResults"
  IO.println s!"{partialResults}"

  let result := partialResults.foldl (· + ·) 0
  IO.println s!"result:   {result}"

  if fileSuffix.toSlice.contains "sample" then
    let contents ← IO.FS.readFile s!"data/day_03_{fileSuffix}_expected_b.txt"
    IO.println s!"expected: {contents}"

#eval! do
  partA "sample"
  partA "real"
  partB "sample"
  partB "real"
