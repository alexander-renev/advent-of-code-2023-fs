module AdventOfCode2023.Solutions.Days.Day06

open System
open System.Text.RegularExpressions
open AdventOfCode2023.Solutions.Common
open AdventOfCode2023.Solutions.Utils

type Race = { Time: int64; Distance: int64 }
type Input = { Races: Race array; BigRace: Race }

let numberRegex = Regex("\\d+", RegexOptions.Compiled)

let parseInput (input: string): Input =
    let lines = getLines input
    let numbers =
        lines
        |> Seq.map (
            fun line ->
                line
                |> numberRegex.Matches
                |> Seq.map (fun m -> Int64.Parse(m.ValueSpan))
                |> Seq.toArray)
        |> Seq.toArray
    let races =
        (numbers[0], numbers[1])
        ||> Array.zip
        |> Seq.map (fun (time, distance) -> { Time = time; Distance = distance })
        |> Seq.toArray
        
    let bigRace =
        lines
        |> Seq.map (
            fun line ->
                line.Replace(" ", String.Empty)
                |> numberRegex.Matches
                |> Seq.map (fun m -> Int64.Parse(m.ValueSpan))
                |> Seq.head)
        |> Seq.toArray
        |> fun ar -> { Time = ar[0]; Distance  = ar[1] }
    { Races = races; BigRace  = bigRace }

type Solution() =
    interface ISolution with
        override this.Input = createInput 6
        
        override this.SolvePart1(input) =
            let parsed = parseInput input
            let product =
                parsed.Races
                |> Seq.map (
                    fun race ->
                        seq { 1L .. race.Time - 1L }
                        |> Seq.filter (fun time -> time * (race.Time - time) > race.Distance)
                        |> Seq.length)
                |> Seq.reduce (*)
            printfn $"Product is {product}"
                                  
        override this.SolvePart2(input) =
            let parsed = parseInput input
            let time =
                seq { 1L .. parsed.BigRace.Time - 1L }
                |> Seq.filter (fun time -> time * (parsed.BigRace.Time - time) > parsed.BigRace.Distance)
                |> Seq.length
            printfn $"Time is {time}"
