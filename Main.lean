import AdventOfCode2025

def main (args : List String) : IO Unit := do
  match args with
  | [day, part] =>
    match day, part with
    | "1", "a" => Day1.partA
    | "1", "b" => Day1.partB
    | "2", "a" => Day2.partA
    | "2", "b" => Day2.partB
    | _, _ => IO.println "Invalid day or part. Usage: adventofcode2025 <day> <part>"
  | _ => IO.println "Usage: adventofcode2025 <day> <part>"
