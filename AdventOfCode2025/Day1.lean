namespace Day1

private def parseLine (line : String) : Option (Char × Int) := do
  -- Get first character (direction)
  let direction ← String.Pos.Raw.get? line 0

  -- Get rest of string (number)
  let numStr := line.drop 1
  let number ← numStr.toNat?

  return (direction, number)

private def readInputFile (fileSuffix : String) : IO (List (Char × Int)) := do
  let fileName := s!"data/day_01_{fileSuffix}.txt"
  let contents ← IO.FS.readFile fileName
  let lines := contents.splitOn "\n"
  let lines := lines.filter (fun line => line.trim != "")

  -- Parse each line into a tuple
  let parsed := lines.filterMap parseLine

  return parsed

private def processInstruction (state : Int × Int) (instruction : Char × Int) : IO (Int × Int) := do
  let (dir, distance) := instruction
  let (posn, count) := state
  let nextMove := match dir with
  | 'R' => posn + distance
  | 'L' => posn - distance
  | _ => posn

  let isZero := match nextMove with
  | m =>
    if m % 100 == 0 then
      1
    else
      0

  IO.println s!"{nextMove}"
  return (nextMove, count + isZero)

def partA (fileSuffix : String) : IO Unit := do
  IO.println "Running Day 1, Part A"
  let instructions ← readInputFile fileSuffix
  let finalState ← instructions.foldlM processInstruction ((50, 0) : Int × Int)
  IO.println s!"Final position: ({finalState})"


def partB (fileSuffix : String) : IO Unit := do
  IO.println "Running Day 1, Part B"
