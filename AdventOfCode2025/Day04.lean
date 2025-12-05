import Mathlib.Data.List.MinMax
import Std.Data.HashSet

namespace Day04

def parse2DGrid (input : String) : Array (Array Char) :=
  input.splitOn "\n"
    |>.filter (· != "")
    |>.map (·.toList.toArray)
    |>.toArray

private def readInputFile (fileSuffix : String) : IO (Array (Array Char)) := do
  let fileName := s!"data/day_04_{fileSuffix}.txt"
  let contents ← IO.FS.readFile fileName
  return parse2DGrid contents

def getOrDefault2D (paperMap : Array (Array Char)) (R : Nat) (C : Nat)  (r : Int) (c : Int) (default : Char): Char :=
  if r >= 0 && r < R && c >= 0 && c < C then
    paperMap[r.toNat]![c.toNat]!
  else
    default

def countPapers (paperMap : Array (Array Char)) (R : Nat) (C : Nat)  (r : Nat) (c :Nat) : Nat :=
  ([
    getOrDefault2D paperMap R C (r-1) (c-1) '.',
    getOrDefault2D paperMap R C (r-1) (c+0) '.',
    getOrDefault2D paperMap R C (r-1) (c+1) '.',
    getOrDefault2D paperMap R C (r+0) (c-1) '.',
    getOrDefault2D paperMap R C (r+0) (c+1) '.',
    getOrDefault2D paperMap R C (r+1) (c-1) '.',
    getOrDefault2D paperMap R C (r+1) (c+0) '.',
    getOrDefault2D paperMap R C (r+1) (c+1) '.',
  ].filter (. =='@')).length

def countForkLiftPositionsRecursion (paperMap : Array (Array Char)) (R : Nat) (C : Nat) (r : Nat) (c : Nat) (acc : Int): Int :=
  match r, c with
  | 0, _ => acc
  | nextRow + 1, 0 => countForkLiftPositionsRecursion paperMap R C nextRow C acc
  | nextRow + 1, nextCol + 1 =>
    let delta := match getOrDefault2D paperMap R C nextRow nextCol '.' with
    | '@' => 0
    | _ =>
      let adjacentPapers := countPapers paperMap R C nextRow nextCol
      if adjacentPapers < 4 then
        1
      else
        0
    countForkLiftPositionsRecursion paperMap R C (nextRow + 1) nextCol (acc+delta)
termination_by (r, c)

def countForkLiftPositions (paperMap : Array (Array Char)) : Int :=
  let R := paperMap.size
  let C := paperMap[0]!.size
  countForkLiftPositionsRecursion paperMap R C R C 0

def partA (fileSuffix : String) : IO Unit := do
  IO.println "Running Day 4, Part A"

  let input ← readInputFile fileSuffix
  IO.println s!"input:\n{input}"

  let result :=  countForkLiftPositions input
  IO.println s!"result:   {result}"

  if fileSuffix.toSlice.contains "sample" then
    let contents ← IO.FS.readFile s!"data/day_04_{fileSuffix}_expected_a.txt"
    IO.println s!"expected: {contents}"



def partB (fileSuffix : String) : IO Unit := do
  IO.println "Running Day 4, Part B"

  let input ← readInputFile fileSuffix

  let result := 0
  IO.println s!"result:   {result}"

  if fileSuffix.toSlice.contains "sample" then
    let contents ← IO.FS.readFile s!"data/day_04_{fileSuffix}_expected_b.txt"
    IO.println s!"expected: {contents}"

#eval! do
  partA "sample"
