import Mathlib.Data.List.MinMax
import Std.Data.HashSet

namespace Day11


private def readInputFile
    (fileSuffix : String)
    : IO (String) := do
  let fileName := s!"data/day_11_{fileSuffix}.txt"
  let contents ← IO.FS.readFile fileName
  return contents


private def parseGraph
    (content : String)
    : Std.HashMap String (List String) :=

    content.splitOn "\n"
    |>.map (fun line => line.splitOn ": ")
    |>.map (fun tokens => (tokens[0]!, tokens[1]!))
    |>.map (fun (key, vals) => (key, vals.splitOn " "))
    |>.foldl (fun acc (key, vals) => acc.insert key vals) ∅


partial def countAllPathsRecurse
    (graph : Std.HashMap String (List String))
    (endNode : String)
    (currentNode : String)
    (visited : Std.HashSet String)
    (depth : Nat)
    (acc : Nat)
    : Nat :=
  let visitedStr := String.intercalate "," visited.toList
  dbg_trace s!"currentNode={currentNode} depth={depth} acc={acc} visited={visitedStr}"
  if currentNode == endNode then
    acc + 1
  else if depth >= 10 then
    acc
  else
    (graph.getD currentNode [])
    |>.filter (fun nextNode => !(visited.contains nextNode))
    |>.foldl (fun acc nextNode =>
      countAllPathsRecurse graph endNode nextNode (visited.insert nextNode) (depth +1) acc
    ) acc

def countAllPaths
    (graph : Std.HashMap String (List String))
    (startNode : String)
    (endNode : String)
    : Nat :=
  countAllPathsRecurse graph endNode startNode ∅ 0 0

def partA
    (fileSuffix : String)
    : IO Unit := do
  let start ← IO.monoMsNow
  IO.println "Running Day 11, Part A"

  let input ← readInputFile fileSuffix
  let graph := parseGraph input
  let graphStr := String.intercalate "\n" (graph.toList.map toString)
  IO.println s!"graph:\n{graphStr}"

  let result := countAllPaths graph "you" "out"
  IO.println s!"result:   {result}"

  if fileSuffix.toSlice.contains "sample" then
    let contents ← IO.FS.readFile s!"data/day_11_{fileSuffix}_expected_a.txt"
    IO.println s!"expected: {contents}"
  let stop ← IO.monoMsNow
  IO.println s!"took {stop - start}ms"

def partB
    (fileSuffix : String)
    : IO Unit := do
  IO.println "Running Day 11, Part B"
  let start ← IO.monoMsNow

  let input ← readInputFile fileSuffix
  let graph := parseGraph input
  let graphStr := String.intercalate "\n" (graph.toList.map toString)
  IO.println s!"graph:\n{graphStr}"

  let result := countAllPaths graph "svr" "out"
  IO.println s!"result:   {result}"

  if fileSuffix.toSlice.contains "sample" then
    let contents ← IO.FS.readFile s!"data/day_11_{fileSuffix}_expected.txt"
    IO.println s!"expected: {contents}"
  let stop ← IO.monoMsNow
  IO.println s!"took {stop - start}ms"

#eval! do
  partA "sample"
  --partA "real"

  partB "sample_b"
  --partB "real"
