module AdventOfCode2023.Solutions.Days.Day09

open System
open AdventOfCode2023.Solutions.Common
open AdventOfCode2023.Solutions.Utils

type Solution() =
    interface ISolution with
        override this.Input = createInput 9
        
        override this.SolvePart1(input) =
            let source = this.ParseInput input
            let sum =
                source
                |> Seq.map this.GetNextValue
                |> Seq.sum
            printfn $"Sum is {sum}"
                                  
        override this.SolvePart2(input) =
            let source = this.ParseInput input
            let sum =
                source
                |> Seq.map this.GetPreviousValue
                |> Seq.sum
            printfn $"Sum is {sum}"
    
    member private this.GetNextValue(samples: int list) =
        samples
        |> this.MakeLists
        |> Seq.map Seq.last
        |> Seq.sum
        
    member private this.GetPreviousValue(samples: int list): int =
        samples
        |> this.MakeLists
        |> Seq.map Seq.head
        |> pairWith 0
        ||> Seq.foldBack (
            fun acc value ->
                acc - value)

    member private this.MakeLists(samples: int list) =
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
                    |> Some
            )
    member private this.ParseInput(input: string) =
        input
        |> getLines
        |> Seq.map(
            fun line ->
                line
                |> _.Split(' ')
                |> Seq.map Int32.Parse
                |> Seq.toList
            )
        |> Seq.toList
