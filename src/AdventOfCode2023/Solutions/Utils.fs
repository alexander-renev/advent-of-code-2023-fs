module AdventOfCode2023.Solutions.Utils

open System

let swapParts(a, b) = (b, a)

let getLines(input: string) = input.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
