module AdventOfCode2023.Solutions.Days.Day13

open System
open System.Collections.Immutable
open AdventOfCode2023.Solutions.Common
open AdventOfCode2023.Solutions.Utils

type Sample = { Rows: char list list; Columns: char list list; }

type Mirror = Horizontal of int | Vertical of int

let parseSample(sample: string) =
    let rows =
        getLines sample
        |> Seq.map (
            fun line ->
                line.ToCharArray()
                |> Seq.toList
            )
        |> Seq.toList
    let columns = rows |> List.transpose
    { Rows = rows; Columns  = columns }
    
let findMiddle(source: char list list): int list =
    let length = source.Length
    seq { 1 .. length - 1 }
    |> Seq.filter (
        fun divisor ->
            let left = source |> Seq.take divisor |> Seq.rev
            let right = source |> Seq.skip divisor
            (left, right) 
            ||> Seq.zip
            |> Seq.forall (fun (x1, x2) -> x1 = x2))
    |> Seq.toList            
    
    
let getCombinations (sourceList: char list list) =
    let source =    
        sourceList
        |> Seq.map ImmutableArray<char>.Empty.AddRange
        |> ImmutableArray<ImmutableArray<char>>.Empty.AddRange
    seq { 0 .. (sourceList.Length - 1) }
    |> Seq.allPairs (seq { 0 .. (sourceList.Head.Length - 1) })
    |> Seq.map (
        fun (x, y) ->
            let value = if source[y][x] = '#' then '.' else '#'
            source.SetItem(y, source[y].SetItem(x, value))
        )
    |> Seq.map (
        fun data ->
            let rows =
                data
                |> Seq.map List.ofSeq
                |> List.ofSeq
            { Rows = rows; Columns = rows |> List.transpose })

let describeLine item =
    let horizontal =
        findMiddle item.Rows
        |> Seq.map Horizontal
    let vertical =
        findMiddle item.Columns
        |> Seq.map Vertical
    [horizontal; vertical]
    |> Seq.collect id
    |> Seq.map (
        fun found ->
            match found with
            | Horizontal x -> x * 100
            | Vertical x -> x)
    |> Seq.toList
    
let parseInput(input: string) =
    input.Split(Environment.NewLine + Environment.NewLine)
    |> Seq.map parseSample
    |> Seq.toList

type Solution() =
    interface ISolution with
        override this.Input = createInput 13
        
        override this.SolvePart1(input) =
            let parsed = parseInput input
            let result =
                parsed
                |> Seq.map (describeLine >> List.exactlyOne)
                |> Seq.sum
            printfn $"Sum is {result}"
                                  
        override this.SolvePart2(input) =
            let parsed = parseInput input
            let result =
                parsed
                |> Seq.map (
                    fun line ->
                        let current = describeLine line |> List.exactlyOne
                        getCombinations line.Rows
                        |> Seq.collect describeLine
                        |> Seq.filter (fun v -> v <> current)
                        |> List.ofSeq
                        |> _.Head)
                |> Seq.sum
            printfn $"Sum is {result}"
