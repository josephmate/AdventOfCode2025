import AdventOfCode2025

def main (args : List String) : IO Unit := do
  match args with
  | [day, part, fileSuffix] =>
    match day, part with
    | "1", "a" => Day01.partA fileSuffix
    | "1", "b" => Day01.partB fileSuffix
    | "2", "a" => Day02.partA fileSuffix
    | "2", "b" => Day02.partB fileSuffix
    | _, _ => IO.println "Invalid day or part. Usage: adventofcode2025 <day> <part>"
  | _ => IO.println "Usage: adventofcode2025 <day> <part> <fileSuffix>"
