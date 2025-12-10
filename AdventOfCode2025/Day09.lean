import Mathlib.Data.List.MinMax
import Std.Data.HashSet

namespace Day09


private def readInputFile
    (fileSuffix : String)
    : IO (String) := do
  let fileName := s!"data/day_09_{fileSuffix}.txt"
  let contents ← IO.FS.readFile fileName
  return contents

structure Coord where
  x : Int
  y : Int
  deriving Repr, BEq, Hashable

instance : ToString Coord where
  toString c := s!"({c.x}, {c.y})"

private def parseRedTiles (content : String) : List (Coord) :=
  content.splitOn "\n"
  |> List.map (fun line => line.splitOn ",")
  |> List.map (fun coordsStr => coordsStr.map (String.toInt!))
  |> List.map (fun coordsList => ⟨coordsList[0]!, coordsList[1]!⟩)

def expandPairs (coord : Coord) (coords : List (Coord)) (acc : List (Coord × Coord)) :=
  match coords with
  | [] => acc
  | head :: tail => expandPairs coord tail ((coord, head) :: acc)

def calcAllUniquePairsRecurse (coords : List (Coord)) (acc : List (Coord × Coord)) : List (Coord × Coord) :=
  match coords with
  | [] => acc
  | head :: tail => calcAllUniquePairsRecurse tail (expandPairs head tail acc)

def calcAllUniquePairs (coords : List (Coord)) : List (Coord × Coord) :=
  calcAllUniquePairsRecurse coords []

def area (a : Coord) (b : Coord) : Int :=
  let (biggerX, smallerX) :=
    if a.x > b.x then
      (a.x, b.x)
    else
      (b.x, a.x)
  let (biggerY, smallerY) :=
    if a.y > b.y then
      (a.y, b.y)
    else
      (b.y, a.y)
  (biggerX - smallerX)*(biggerY - smallerY)

def calcAllAreas (coords : List (Coord)) : List (Int × Coord × Coord) :=
  (
  calcAllUniquePairs (coords)
  |> List.map (fun (a, b) => ((area a b), a, b))
  )


def maxRecurse
    (areas : List (Int × Coord × Coord))
    (acc : Int × Coord × Coord)
    : (Int × Coord × Coord) :=
  match areas with
  | [] => acc
  | (area, a, b) :: tail =>
    let (maxSoFar, _, _) := acc
    let newAcc :=
      if area > maxSoFar then
        (area, a, b)
      else
        acc
    maxRecurse tail newAcc


def max
    (areas : List (Int × Coord × Coord))
    : (Int × Coord × Coord) :=
  maxRecurse areas (-1, ⟨ -1, -1 ⟩, ⟨ -1, -1 ⟩)

def calcBiggestArea
    (redTiles : List (Coord))
    : (Int × Coord × Coord) :=
  calcAllAreas redTiles
  |> max

def partA
    (fileSuffix : String)
    : IO Unit := do
  let start ← IO.monoMsNow
  IO.println "Running Day 9, Part A"

  let input ← readInputFile fileSuffix
  let redTiles := parseRedTiles input
  IO.println s!"redTiles:   {redTiles}"

  let (result, a, b) := calcBiggestArea redTiles
  IO.println s!"a b:   {a} {b}"
  IO.println s!"result:   {result}"

  if fileSuffix.toSlice.contains "sample" then
    let contents ← IO.FS.readFile s!"data/day_09_{fileSuffix}_expected_a.txt"
    IO.println s!"expected: {contents}"
  let stop ← IO.monoMsNow
  IO.println s!"took {stop - start}ms"

def partB
    (fileSuffix : String)
    : IO Unit := do
  IO.println "Running Day 9, Part B"
  let start ← IO.monoMsNow

  let input ← readInputFile fileSuffix

  let result := 0
  IO.println s!"result:   {result}"

  if fileSuffix.toSlice.contains "sample" then
    let contents ← IO.FS.readFile s!"data/day_09_{fileSuffix}_expected_b.txt"
    IO.println s!"expected: {contents}"
  let stop ← IO.monoMsNow
  IO.println s!"took {stop - start}ms"

#eval! do
  partA "sample"
