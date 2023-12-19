module AdventOfCode2023.Solutions.Days.Day03

open System
open System.Text.RegularExpressions
open System.Collections.Generic
open AdventOfCode2023.Solutions.Common
open AdventOfCode2023.Solutions.Utils

[<Struct>]
type Point = { X: int; Y: int; }

type InputData = { Lines: string list; Symbols: Point list; Asterisks: Point list }

let digitRegex = Regex("\\d+", RegexOptions.Compiled)
    
let ignoreChars = HashSet<char>("0123456789.".ToCharArray())

let parseInput(input: string) =
    let lines = getLines input |> Seq.toList
    
    let source =
        lines
        |> Seq.mapi (fun y line -> (y, line))
        |> Seq.collect (
            fun (y, line) ->
                line |>
                Seq.mapi (fun x ch -> ({X = x; Y = y}, ch)))
        
    let symbols =
        source
        |> Seq.filter (fun p -> ignoreChars.Contains(snd p) = false)
        |> Seq.map fst
        |> Seq.toList
                  
    let asterisks =
        source
        |> Seq.filter (fun p -> snd p = '*')
        |> Seq.map fst
        |> Seq.toList

    { Lines = lines; Symbols = symbols; Asterisks = asterisks }

type Solution() =
    interface ISolution with
        override this.Input = createInput 3
        
        override this.SolvePart1(input) =
            let parsed = parseInput input
            let sum =
                parsed.Lines
                |> Seq.mapi (
                    fun y line ->
                        let matches = digitRegex.Matches line
                        matches
                        |> Seq.filter (
                            fun mtch ->
                                parsed.Symbols
                                |> Seq.exists (
                                    fun smbl ->
                                        smbl.Y >= y - 1 &&
                                        smbl.Y <= y + 1 &&
                                        smbl.X >= mtch.Index - 1 &&
                                        smbl.X <= mtch.Index + mtch.Length                                        
                                    )
                            )
                        |> Seq.map (fun mtch -> Int64.Parse(mtch.Value))
                    )
                |> Seq.collect id
                |> Seq.sum
            printfn $"Sum is {sum}"
                                  
        override this.SolvePart2(input) =
            let parsed = parseInput input
            let gears =
                parsed.Lines
                |> Seq.mapi (
                    fun y line ->
                        digitRegex.Matches line
                        |> fun matches ->
                            parsed.Asterisks
                            |> Seq.collect (
                                fun gear ->
                                    matches
                                    |> Seq.filter(
                                        fun mtch ->
                                            gear.Y >= y - 1 &&
                                            gear.Y <= y + 1 &&
                                            gear.X >= mtch.Index - 1 &&
                                            gear.X <= mtch.Index + mtch.Length)
                                    |> Seq.map (fun mtch -> (gear, Int64.Parse(mtch.Value)))
                                )
                        
                    )
                |> Seq.collect id
                |> Seq.groupBy fst
                |> Seq.filter (fun (_, values) -> Seq.length(values) = 2)
                |> Seq.map snd
                |> Seq.sumBy (
                    fun nums ->
                        nums
                        |> Seq.map snd
                        |> Seq.reduce (*)
                        )
            printfn $"Sum is {gears}"
