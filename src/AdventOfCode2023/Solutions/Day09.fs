module AdventOfCode2023.Solutions.Days.Day09

open System
open AdventOfCode2023.Solutions.Common
open AdventOfCode2023.Solutions.Utils


let makeLists(samples: int list) =
    samples
    |> Seq.unfold (
        fun list ->
            if Seq.forall (fun x -> x = 0) list
                then None
            else
                list
                |> Seq.pairwise
                |> Seq.map (fun (x1, x2) -> x2 - x1)
                |> Seq.toList
                |> reversePairWith list
                |> Some)

let getNextValue(samples: int list) =
    samples
    |> makeLists
    |> Seq.map Seq.last
    |> Seq.sum
    
let getPreviousValue(samples: int list): int =
    samples
    |> makeLists
    |> Seq.map Seq.head
    |> pairWith 0
    ||> Seq.foldBack (
        fun acc value -> acc - value)

let ParseInput(input: string) =
    input
    |> getLines
    |> Seq.map(
        fun line ->
            line
            |> _.Split(' ')
            |> Seq.map Int32.Parse
            |> Seq.toList)
    |> Seq.toList

type Solution() =
    interface ISolution with
        override this.Input = createInput 9
        
        override this.SolvePart1(input) =
            let source = ParseInput input
            let sum =
                source
                |> Seq.map getNextValue
                |> Seq.sum
            printfn $"Sum is {sum}"
                                  
        override this.SolvePart2(input) =
            let source = ParseInput input
            let sum =
                source
                |> Seq.map getPreviousValue
                |> Seq.sum
            printfn $"Sum is {sum}"
