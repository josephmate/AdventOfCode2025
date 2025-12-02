import AdventOfCode2025

def main (args : List String) : IO Unit := do
  match args with
  | [day, part, fileSuffix] =>
    match day, part with
    | "1", "a" => Day1.partA fileSuffix
    | "1", "b" => Day1.partB fileSuffix
    | "2", "a" => Day2.partA fileSuffix
    | "2", "b" => Day2.partB fileSuffix
    | _, _ => IO.println "Invalid day or part. Usage: adventofcode2025 <day> <part>"
  | _ => IO.println "Usage: adventofcode2025 <day> <part> <fileSuffix>"
