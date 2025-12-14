import Mathlib.Data.List.MinMax
import Std.Data.HashSet

namespace Day12


private def readInputFile
    (fileSuffix : String)
    : IO (String) := do
  let fileName := s!"data/day_12_{fileSuffix}.txt"
  let contents ← IO.FS.readFile fileName
  return contents

structure Request where
  rows : Nat
  cols : Nat
  amounts : List Nat
  deriving Repr, BEq, Hashable

instance : ToString Request where
  toString c := s!"rows={c.rows},cols={c.cols}\namounts=\n{c.amounts}\n"

structure Problem where
  presents : Array Nat
  requests : List Request
  deriving Repr, BEq, Hashable

instance : ToString Problem where
  toString c := s!"presents={c.presents}\nrequests={c.requests}"

private def parseRequest
    (content : String)
    : Request :=
  let tokens := content.splitOn ": "
  let front := tokens[0]!
  let back := tokens[1]!
  let dims := front.splitOn "x"
  let rows := dims[0]!.toNat!
  let cols := dims[1]!.toNat!

  let requests := back.splitOn " "
  |>.map String.toNat!

  ⟨ rows, cols, requests ⟩

private def parseRequests
    (content : String)
    : List Request :=

  content.splitOn "\n"
  |>.map parseRequest

private def parseInput
    (content : String)
    : Problem :=
  let chunks := content.splitOn "\n\n"

  let presents := chunks.take 6
  |>.map (fun present => present.toList.filter (· == '#') |>.length)
  |>.toArray

  ⟨ presents , parseRequests chunks.getLast! ⟩

def partA
    (fileSuffix : String)
    : IO Unit := do
  let start ← IO.monoMsNow
  IO.println "Running Day 12, Part A"

  let input ← readInputFile fileSuffix
  let problem := parseInput input
  IO.println s!"problem:\n{problem}"

  let result := 0
  IO.println s!"result:   {result}"

  if fileSuffix.toSlice.contains "sample" then
    let contents ← IO.FS.readFile s!"data/day_12_{fileSuffix}_expected_a.txt"
    IO.println s!"expected: {contents}"
  let stop ← IO.monoMsNow
  IO.println s!"took {stop - start}ms"

def partB
    (fileSuffix : String)
    : IO Unit := do
  IO.println "Running Day 12, Part B"
  let start ← IO.monoMsNow

  let input ← readInputFile fileSuffix

  let result := 0
  IO.println s!"result:   {result}"

  if fileSuffix.toSlice.contains "sample" then
    let contents ← IO.FS.readFile s!"data/day_12_{fileSuffix}_expected_b.txt"
    IO.println s!"expected: {contents}"
  let stop ← IO.monoMsNow
  IO.println s!"took {stop - start}ms"

#eval! do
  partA "sample"
