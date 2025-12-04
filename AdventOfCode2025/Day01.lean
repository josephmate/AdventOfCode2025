namespace Day01

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
  if fileSuffix.toSlice.contains "sample" then
    let contents ← IO.FS.readFile s!"data/day_01_{fileSuffix}.txt"
    IO.println s!"expected position: ({contents})"



private def processInstructionB (state : Int × Int) (instruction : Char × Int) : IO (Int × Int) := do
  let (dir, distance) := instruction
  let (posn, count) := state
  let nextMove := match dir with
  | 'R' => posn + distance
  | 'L' => posn - distance
  | _ => posn



  let overages :=
    if nextMove > 0 then
      nextMove / 100
    else
      -1 * nextMove / 100

  IO.println s!"{nextMove}, {overages}"

  let (fixedMove, shouldCount) := match nextMove with
  | m =>
    if m >= 100 then
      (m-100*overages, overages)
    else if m < 0 then
      let next :=
        if m % 100 == 0 then
          m+100*(overages+0)
        else
          m+100*(overages+1)
      if posn == 0 then
        (next, overages) -- already counted in prev iteration
      else
        (next, overages+1)
    else if m == 0 then
      (m, overages + 1)
    else
      (m, 0)

  return (fixedMove, count + shouldCount)


def partB (fileSuffix : String) : IO Unit := do
  IO.println "Running Day 1, Part A"
  let instructions ← readInputFile fileSuffix
  let finalState ← instructions.foldlM processInstructionB ((50, 0) : Int × Int)
  IO.println s!"Final position: ({finalState})"
  if fileSuffix.toSlice.contains "sample" then
    let contents ← IO.FS.readFile s!"data/day_01_{fileSuffix}_expected.txt"
    IO.println s!"expected position: ({contents})"

private def testInstr (name : String) (state : Int × Int) (instr : Char × Int) (expected : Int × Int) : IO Unit := do
    let actual ← processInstructionB state instr
    if actual == expected then
      IO.println s!"✓ {name}"
    else
      IO.println s!"✗ {name}"
      IO.println s!"  Expected: {expected}"
      IO.println s!"  Actual:   {actual}"

#eval do
  IO.println "=== Day1 Tests ==="
  --      ______________________________________________________________
  --     /    /    /       / / /          |      \   \   \      \   \   \
  --  -101  -100 -99      -1 0 1         50      99 100 101    199 200 201
  testInstr     "R49" (50, 0) ('R',   49) (99,  0)
  testInstr     "R50" (50, 0) ('R',   50) ( 0,  1)
  testInstr     "R51" (50, 0) ('R',   51) ( 1,  1)
  testInstr "R50,  99" (50, 0) ('R',50+99) (99, 1)
  testInstr "R50, 100" (50, 0) ('R',50+100) (0, 2)
  testInstr "R50, 101" (50, 0) ('R',50+101) (1, 2)
  testInstr     "L49" (50, 0) ('L',   49) ( 1,  0)
  testInstr     "L50" (50, 0) ('L',   50) ( 0,  1)
  testInstr     "L51" (50, 0) ('L',   51) (99,  1)
  testInstr "L50,  99" (50, 0) ('L',50+99) (1, 1)
  testInstr "L50, 100" (50, 0) ('L',50+100) (0, 2)
  testInstr "L50, 101" (50, 0) ('L',50+101) (99, 2)
  -- samples provided by AoC
  testInstr   "R1000" (50, 0) ('R', 1000) (50, 10)
  testInstr   "L1000" (50, 0) ('L', 1000) (50, 10)
  IO.println "Tests complete!"
end Day01
