import Mathlib.Data.List.MinMax
import Std.Data.HashSet

namespace Day04


def convertToSetOfTuplesRecurse (paperMap : Array (Array Char)) (R : Nat) (C : Nat) (r : Nat) (c : Nat) (acc : Std.HashSet (Int × Int)) : Std.HashSet (Int × Int) :=
  match r, c with
  | 0, _ => acc
  | r' + 1, 0 =>
    convertToSetOfTuplesRecurse paperMap R C r' C acc
  | r' + 1, c' + 1 =>
    let nextSet :=
      if paperMap[r']![c']! == '@' then
        acc.insert (r', c')
      else
        acc
    convertToSetOfTuplesRecurse paperMap R C (r' + 1) c' nextSet

def convertToSetOfTuples (paperMap : Array (Array Char)) (R : Nat) (C : Nat) : Std.HashSet (Int × Int) :=
  convertToSetOfTuplesRecurse paperMap R C R C ∅

def parse2DGrid (input : String) : Array (Array Char) :=
  input.splitOn "\n"
    |>.filter (· != "")
    |>.map (·.toList.toArray)
    |>.toArray

private def readInputFile (fileSuffix : String) : IO (Array (Array Char)) := do
  let fileName := s!"data/day_04_{fileSuffix}.txt"
  let contents ← IO.FS.readFile fileName
  return parse2DGrid contents

def countPapers (paperMap : Std.HashSet (Int × Int))  (r : Int) (c : Int) : Nat :=
  ([
    ((r-1), (c-1)),
    ((r-1), (c+0)),
    ((r-1), (c+1)),
    ((r+0), (c-1)),
    ((r+0), (c+1)),
    ((r+1), (c-1)),
    ((r+1), (c+0)),
    ((r+1), (c+1)),
  ].filter (paperMap.contains ·)).length

def enoughPapers (paperMap : Std.HashSet (Int × Int)) (paperPosn : (Int × Int)) : Bool :=
  let (r, c) := paperPosn
  paperMap.contains paperPosn && (countPapers paperMap r c) < 4

def findForkLiftPositions (paperMap : Std.HashSet (Int × Int)) : Std.HashSet (Int × Int) :=
  paperMap.filter (enoughPapers paperMap)

def printMapRecurse (paperMap : Std.HashSet (Int × Int)) (posns : Std.HashSet (Int × Int)) (R : Nat) (C : Nat) (r : Nat) (c : Nat): IO Unit := do
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
      else if paperMap.contains (r'', c'') then
        '@'
      else
        '.'
    IO.print char
    printMapRecurse paperMap posns R C (r' + 1) c'

termination_by (r, c)

def printMap (paperMap : Std.HashSet (Int × Int)) (posns : Std.HashSet (Int × Int)) (R : Nat) (C : Nat): IO Unit := do
  printMapRecurse paperMap posns R C R C

def partA (fileSuffix : String) : IO Unit := do
  IO.println "Running Day 4, Part A"

  let input ← readInputFile fileSuffix
  IO.println s!"input:\n{input}"
  let R := input.size
  let C := input[0]!.size
  let paperMap := convertToSetOfTuples input R C

  let potentialPositions := findForkLiftPositions paperMap
  printMap paperMap potentialPositions R C

  let result := potentialPositions.size
  IO.println s!"result:   {result}"

  if fileSuffix.toSlice.contains "sample" then
    let contents ← IO.FS.readFile s!"data/day_04_{fileSuffix}_expected_a.txt"
    IO.println s!"expected: {contents}"


partial def removeUntilCannotRecurse (paperMap : Std.HashSet (Int × Int)) (acc : Std.HashSet (Int × Int)): Std.HashSet (Int × Int) :=
  let potentialPositions := findForkLiftPositions paperMap
  if potentialPositions.isEmpty then
    acc
  else
    let newAcc := potentialPositions.fold (fun set elem => set.insert elem) acc
    let reducedMap := potentialPositions.fold (fun set elem => set.erase elem) paperMap
    removeUntilCannotRecurse reducedMap newAcc


def removeUntilCannot (paperMap : Std.HashSet (Int × Int)) : Std.HashSet (Int × Int) :=
  removeUntilCannotRecurse paperMap ∅

def partB (fileSuffix : String) : IO Unit := do
  IO.println "Running Day 4, Part B"

  let input ← readInputFile fileSuffix
  let R := input.size
  let C := input[0]!.size
  let paperMap := convertToSetOfTuples input R C

  let result := (removeUntilCannot paperMap).size
  IO.println s!"result:   {result}"

  if fileSuffix.toSlice.contains "sample" then
    let contents ← IO.FS.readFile s!"data/day_04_{fileSuffix}_expected_b.txt"
    IO.println s!"expected: {contents}"

#eval! do
  partA "sample"
  partA "real"
  partB "sample"
  partB "real"
