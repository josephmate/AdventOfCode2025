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

def countForkLiftPositionsRecursion (paperMap : Array (Array Char)) (R : Nat) (C : Nat) (r : Nat) (c : Nat) (acc : List (Nat × Nat)): List (Nat × Nat) :=
  match r, c with
  | 0, _ => acc
  | nextRow + 1, 0 => countForkLiftPositionsRecursion paperMap R C nextRow C acc
  | nextRow + 1, nextCol + 1 =>
    let adjacentPapers := countPapers paperMap R C nextRow nextCol
    let newList :=
      if adjacentPapers < 4 then
        (nextRow, nextCol) :: acc
      else
        acc
    countForkLiftPositionsRecursion paperMap R C (nextRow + 1) nextCol newList
termination_by (r, c)

def printMapRecurse(paperMap : Array (Array Char)) (posns : Std.HashSet (Nat × Nat)) (R : Nat) (C : Nat)(r : Nat) (c : Nat): IO Unit := do
  match r, c with
  | 0, _ => return
  | r' + 1, 0 =>
    IO.println ""
    printMapRecurse paperMap posns R C r' C
  | r' + 1, c' + 1 =>
    let r'' := R - r' - 1
    let c'' := C - c' -1
    let char :=
      if posns.contains (r'', c'') then
        'x'
      else
        paperMap[r'']![c'']!
    IO.print char
    printMapRecurse paperMap posns R C (r' + 1) c'

termination_by (r, c)

def printMap(paperMap : Array (Array Char)) (posns: List (Nat × Nat)) : IO Unit := do
  let R := paperMap.size
  let C := paperMap[0]!.size
  printMapRecurse paperMap (Std.HashSet.ofList posns) R C R C

def countForkLiftPositions (paperMap : Array (Array Char)) : List (Nat × Nat) :=
  let R := paperMap.size
  let C := paperMap[0]!.size
  countForkLiftPositionsRecursion paperMap R C R C []

def partA (fileSuffix : String) : IO Unit := do
  IO.println "Running Day 4, Part A"

  let input ← readInputFile fileSuffix
  IO.println s!"input:\n{input}"

  let potentialPositions := (countForkLiftPositions input)
  printMap input potentialPositions

  let result := potentialPositions.length
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
