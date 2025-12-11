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
  original : String
  target : Array Bool
  toggles : Array (List Nat)
  joltages : Array Nat
  deriving Repr, BEq, Hashable

instance : ToString MachineSetup where
  toString c := s!"original={c.original}\n\ttarget={c.target}\n\ttoggles={c.toggles}\n\tjoltages{c.joltages})"

def parseTarget
    (tokens : List String)
    : Array Bool :=
  --dbg_trace s!"tokens[0]!={tokens[0]!}"
  let leftSquare := tokens[0]!.replace "[" ""
  let rightSquare := leftSquare.replace "]" ""
  let result := rightSquare.toList
  |> List.map (fun c =>
    if c == '.' then false
    else true
  )
  |> List.toArray
  --dbg_trace s!"result={result}"
  result


def parseToggles
    (tokens : List String)
    : Array (List Nat) :=
  let sublist :=
    tokens.drop 2
    |> List.dropLast
  --dbg_trace s!"sublist={sublist}"

  let parensDropped : List String := sublist.map (fun token => token.replace "(" "")
    |> List.map (fun token => token.replace ")" "")
  -- dbg_trace s!"parensDropped={parensDropped}"


  parensDropped.map (fun token => token.splitOn ",")
  |> List.map (fun natStrs => natStrs.map (String.toNat!))
  |> List.toArray

def trace {α : Type} [ToString α] (msg : String) (x : α) : α :=
  -- dbg_trace s!"{msg}: {x}"
  x

def parseJoltages
    (tokens : List String)
    : Array Nat :=
  let lastToken := tokens.getLast!
  --dbg_trace s!"lastToken={lastToken}"
  let frontReplaced := lastToken.replace "{" ""
  --dbg_trace s!"frontReplaced={frontReplaced}"
  let endReplaced := frontReplaced.replace "}" ""
  --dbg_trace s!"endReplaced={endReplaced}"
  endReplaced.splitOn ","
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
      line,
      parseTarget tokens,
      parseToggles tokens,
      parseJoltages tokens,
    ⟩
  )

partial def calcLights
    (machineSetup : MachineSetup)
    (choice : Nat)
    (lights : Array Bool)
    : Array Bool :=
  machineSetup.toggles[choice]!.foldl (fun acc idx =>
    acc.set! idx (!acc[idx]!)
  ) lights

partial def calcMinRecurse
    (machineSetup : MachineSetup)
    (lights : Array Bool)
    (idx : Nat)
    (soFar : Std.HashSet Nat)
    (acc : Option Nat)
    : Option Nat :=
  dbg_trace s!"lights={lights} idx={idx} soFar={soFar.toList} acc={acc}"
  if (lights == machineSetup.target) && ((acc == none) || soFar.size < acc.get!) then
    some soFar.size
  else if idx >= machineSetup.toggles.size then
    acc
  else
    -- use ti
    let choice1 :=
      calcMinRecurse
        machineSetup
        (calcLights machineSetup idx lights)
        (idx + 1)
        (soFar.insert idx)
        acc
    -- don't use it
    calcMinRecurse
      machineSetup
      lights
      (idx + 1)
      soFar
      choice1



def calcMin (machineSetup : MachineSetup) : Option Nat :=
  let size := machineSetup.target.size
  calcMinRecurse
    machineSetup
    (List.replicate size false |>.toArray)
    0
    ∅
    none

-- 348 was too low
def partA
    (fileSuffix : String)
    : IO Unit := do
  let start ← IO.monoMsNow
  IO.println "Running Day 10, Part A"

  let input ← readInputFile fileSuffix
  let machineSetups:= parseInput input
  --IO.println s!"machineSetups:   {machineSetups}"

  let solutions := machineSetups.map (fun machineSetup => (machineSetup, calcMin machineSetup))
  let unsolved := solutions.filter (fun (_, val) => val == none)
  let unsolvedStr := String.intercalate "\n" (unsolved.map toString)
  IO.println s!"unsolved:\n{unsolvedStr}"
  let result := solutions.map (fun (_, val) => val) |> List.filterMap id |> List.sum
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
  --partA "sample"
  --partA "real"

  let failing : MachineSetup :=
    ⟨
      "#..#.#.#. (1,3) (1,8) (0,6,7,8) (0,2,3,7,8) (1,2,4,5,6) (0,2,3,5,6,7,8) (1,2,3,5,6,7,8) {41,41,44,60,4,28,36,48,58}",
      #[true, false, false, true, false, true, false, true, false],
      #[[1, 8], [0, 6, 7, 8], [0, 2, 3, 7, 8], [1, 2, 4, 5, 6], [0, 2, 3, 5, 6, 7, 8], [1, 2, 3, 5, 6, 7, 8]],
      #[41, 41, 44, 60, 4, 28, 36, 48, 58],
    ⟩

  calcMin failing
