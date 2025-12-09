import Mathlib.Data.List.MinMax
import Std.Data.HashSet

namespace Day08


private def readInputFile (fileSuffix : String) : IO (String) := do
  let fileName := s!"data/day_08_{fileSuffix}.txt"
  let contents ← IO.FS.readFile fileName
  return contents

structure Coord where
  x : Int
  y : Int
  z : Int
  deriving Repr, BEq, Hashable

instance : ToString Coord where
  toString c := s!"({c.x}, {c.y}, {c.z})"

private def parseJunctionCords (content : String) : List (Coord) :=
  content.splitOn "\n"
  |> List.map (fun line => line.splitOn ",")
  |> List.map (fun coordsStr => coordsStr.map (String.toInt!))
  |> List.map (fun coordsList => ⟨coordsList[0]!, coordsList[1]!, coordsList[2]!⟩)

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

def distance (a : Coord) (b : Coord) : Int :=
  (
      (a.x - b.x)*(a.x - b.x)
    + (a.y - b.y)*(a.y - b.y)
    + (a.x - b.z)*(a.z - b.z)
  )

def calcAllDistances (coords : List (Coord)) : List (Int × Coord × Coord) :=
  (
  calcAllUniquePairs (coords)
  |> List.map (fun (a, b) => ((distance a b), a, b))
  )


structure ConnectedComponents where
  coordToComponent : (Std.HashMap Coord Nat)
  componentToCoord : (Std.HashMap Nat (Std.HashSet Coord))

def initConnectedComponents (coords : List (Coord)) : ConnectedComponents :=

  let coordToComponent := coords.zipIdx
  |> Std.HashMap.ofList

  let componentToCoord := coords.zipIdx
  |> List.map (fun (a, b) => (b, {a}))
  |> Std.HashMap.ofList

  ⟨ coordToComponent , componentToCoord ⟩



def findNextClosestNotConnected
    (sortedDistance : List (Int × Coord × Coord))
    (connectedComponents : ConnectedComponents)
    : (Coord × Coord × List (Int × Coord × Coord)) :=
  match sortedDistance with
  | [] => ( ⟨ -1, -1, -1 ⟩, ⟨ -1, -1, -1 ⟩, [])
  | (_, a, b) :: tail =>
    --i suspect I didn't need this check
    --if connectedComponents.coordToComponent.get! a != connectedComponents.coordToComponent.get! b then
      (a, b, tail)
    --else
    --  findNextClosestNotConnected tail connectedComponents

def addConnection
    (a : Coord)
    (b : Coord)
    (connectedComponents : ConnectedComponents)
    : ConnectedComponents :=
  let idDestination := connectedComponents.coordToComponent.get! a
  let idSource := connectedComponents.coordToComponent.get! b
  let coordsToMove := connectedComponents.componentToCoord.get! idSource
  let coordsToMoveTo := connectedComponents.componentToCoord.get! idDestination

  let newCoordToComponent := coordsToMove.toList.foldl (fun acc val =>
    acc.insert val idDestination
  ) connectedComponents.coordToComponent

  let combined := coordsToMoveTo.insertMany coordsToMove
  let erased := connectedComponents.componentToCoord.erase idSource
  let newComponentToCoord := erased.insert idDestination combined

  ⟨ newCoordToComponent, newComponentToCoord ⟩

def connectNJunctionsRecurse
    (coords : List (Coord))
    (sortedDistance : List (Int × Coord × Coord))
    (numJunctions : Nat)
    (connectedComponents : ConnectedComponents)
    : (ConnectedComponents) :=
  match numJunctions with
  | 0 => connectedComponents
  | n + 1 =>
    let (a, b, rest) := findNextClosestNotConnected sortedDistance connectedComponents
    let newConnectedComponents := addConnection a b connectedComponents
    connectNJunctionsRecurse coords rest n newConnectedComponents

def connectNJunctions
    (coords : List (Coord))
    (sortedDistance : List (Int × Coord × Coord))
    (numJunctions : Nat)
    : ConnectedComponents :=
  connectNJunctionsRecurse coords sortedDistance numJunctions (initConnectedComponents coords)

def partA (fileSuffix : String) (numConnections : Nat): IO Unit := do
  let start ← IO.monoMsNow
  IO.println "Running Day 8, Part A"

  let input ← readInputFile fileSuffix
  IO.println s!"input:\n{input}"

  let junctionCoords := parseJunctionCords input
  IO.println s!"junctionCoords:\n{junctionCoords}"

  let allDistances := calcAllDistances junctionCoords
  --IO.println s!"allDistances:\n{allDistances}"

  let sortedDistances := allDistances.mergeSort (fun a b => a.1 < b.1)
  --IO.println s!"sortedDistances:\n{sortedDistances}"

  let connectedComponents := connectNJunctions junctionCoords sortedDistances numConnections
  IO.println s!"connectedComponents:\n{connectedComponents.coordToComponent.toList}"

  let componentsBySize := connectedComponents.componentToCoord.toList
  |> List.map (fun (id, coords) => (coords.size, id ,coords))
  |> List.mergeSort (le := fun a b => b.1 < a.1)

  let printable := String.intercalate "\n" (componentsBySize.map (fun (a,b,c) => s!"{toString a}, {toString b},{toString c.toList}"))
  IO.println s!"componentsBySize:\n{printable}"
  IO.println s!"componentsBySize[0]!.1:   {componentsBySize[0]!.1}"
  IO.println s!"componentsBySize[1]!.1:   {componentsBySize[1]!.1}"
  IO.println s!"componentsBySize[2]!.1:   {componentsBySize[2]!.1}"

  let result := componentsBySize[0]!.1 * componentsBySize[1]!.1 * componentsBySize[2]!.1
  IO.println s!"result:   {result}"

  let stop ← IO.monoMsNow
  IO.println s!"took {stop - start}ms"

  if fileSuffix.toSlice.contains "sample" then
    let contents ← IO.FS.readFile s!"data/day_08_{fileSuffix}_expected_a.txt"
    IO.println s!"expected: {contents}"

def partB (fileSuffix : String) : IO Unit := do
  IO.println "Running Day 8, Part B"

  let input ← readInputFile fileSuffix

  let result := 0
  IO.println s!"result:   {result}"

  if fileSuffix.toSlice.contains "sample" then
    let contents ← IO.FS.readFile s!"data/day_08_{fileSuffix}_expected_b.txt"
    IO.println s!"expected: {contents}"


#eval! do
  partA "sample" 10
  --partA "real" 1000
