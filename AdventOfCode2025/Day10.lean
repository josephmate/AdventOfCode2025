import Mathlib.Data.List.MinMax
import Std.Data.HashSet

namespace Day10


private def readInputFile
    (fileSuffix : String)
    : IO (String) := do
  let fileName := s!"data/day_10_{fileSuffix}.txt"
  let contents ← IO.FS.readFile fileName
  return contents

structure MachineSetup where
  target : Array Bool
  toggles : Array (List Nat)
  joltages : Array Nat
  deriving Repr, BEq, Hashable

instance : ToString MachineSetup where
  toString c := s!"(target={c.target}, toggles={c.toggles} joltages{c.joltages})"

def parseTarget
    (tokens : List String)
    : Array Bool :=
  dbg_trace s!"tokens[0]!={tokens[0]!}"
  let result := tokens[0]!
  |> String.replace "[" ""
  |> String.replace "]" ""
  |> String.toList
  |> List.map (fun c =>
    if c == '.' then false
    else true
  )
  |> List.toArray
  dbg_trace s!"result={result}"
  result


def parseToggles
    (tokens : List String)
    : Array (List Nat) :=
  let sublist :=
    tokens.drop 2
    |> List.dropLast
  dbg_trace s!"sublist={sublist}"

  let parensDropped := sublist.map (fun token => token.replace "(" "" |> String.replace ")" "")
  dbg_trace s!"parensDropped={parensDropped}"


  parensDropped.map (fun token => token.splitOn ",")
  |> List.map (fun natStrs => natStrs.map (String.toNat!))
  |> List.toArray

def parseJoltages
    (tokens : List String)
    : Array Nat :=
  tokens.getLast!
  |> String.replace "{" ""
  |> String.replace "}" ""
  |> String.splitOn ","
  |> List.map (String.toNat!)
  |> List.toArray

-- [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
-- [...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
-- [.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
private def parseInput
    (input : String)
    : List MachineSetup :=
  input.splitOn "\n"
  |> List.map (fun line =>
    let tokens := line.splitOn " "
    ⟨
      parseTarget tokens,
      parseToggles tokens,
      parseJoltages tokens,
    ⟩
  )

def partA
    (fileSuffix : String)
    : IO Unit := do
  let start ← IO.monoMsNow
  IO.println "Running Day 10, Part A"

  let input ← readInputFile fileSuffix
  let machineSetup := parseInput input
  IO.println s!"machineSetup:   {machineSetup}"

  let result := 0
  IO.println s!"result:   {result}"

  if fileSuffix.toSlice.contains "sample" then
    let contents ← IO.FS.readFile s!"data/day_10_{fileSuffix}_expected_a.txt"
    IO.println s!"expected: {contents}"
  let stop ← IO.monoMsNow
  IO.println s!"took {stop - start}ms"

def partB
    (fileSuffix : String)
    : IO Unit := do
  IO.println "Running Day 10, Part B"
  let start ← IO.monoMsNow

  let input ← readInputFile fileSuffix

  let result := 0
  IO.println s!"result:   {result}"

  if fileSuffix.toSlice.contains "sample" then
    let contents ← IO.FS.readFile s!"data/day_10_{fileSuffix}_expected_b.txt"
    IO.println s!"expected: {contents}"
  let stop ← IO.monoMsNow
  IO.println s!"took {stop - start}ms"

#eval! do
  partA "sample"
