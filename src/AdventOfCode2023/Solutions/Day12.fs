module AdventOfCode2023.Solutions.Days.Day12

open System
open System.Collections.Generic
open FSharpPlus
open AdventOfCode2023.Solutions.Common
open AdventOfCode2023.Solutions.Utils

type State = Operational | Damaged | Unknown

type Line = { States: State list; Numbers: int list }

let printList (list: 'a seq) = $"""[{String.Join("; ", list)}]"""

let toLinkedList(source: 'a seq) =
    let result = LinkedList<_>()
    source
    |> Seq.iter (fun x -> result.AddLast x |> ignore)
    result

let parseInput (input: string) =
    input
    |> getLines
    |> Seq.map (
        fun line ->
            let parts = line.Split(' ')
            let springs =
                parts[0].ToCharArray()
                |> Seq.map (
                    fun ch ->
                        match ch with
                        | '?' -> Unknown
                        | '#' -> Damaged
                        | '.' -> Operational
                        | _ -> failwith $"Unknown state {ch}"
                    )
                |> Seq.toList
            let numbers =
                parts[1].Split(',')
                |> Seq.map Int32.Parse
                |> Seq.toList
            { States = springs; Numbers = numbers }
        )
    |> Seq.toList

let cache = Dictionary<Line, int64>()
let rec calculateVariantsNumber (line: Line): int64 =
    let found, result = cache.TryGetValue line
    if found then result
    else
        if List.isEmpty line.Numbers then
            let result = if line.States |> List.forall (fun state -> state <> Damaged) then 1 else 0
            cache.Add(line, result)
            result
        elif List.isEmpty line.States then 0
        else
            let skipped = if line.States.Head = Damaged then 0L else calculateVariantsNumber { line with States = line.States.Tail }
            let count = line.Numbers.Head
            let remaining = List.length(line.States)
            let notSkipped =
                if remaining >= count && line.States |> Seq.take count |> Seq.forall (fun x -> x <> Operational) then
                    if remaining = count then
                        if List.length(line.Numbers) = 1 then 1L else 0L
                    else
                        if line.States |> Seq.skip count |> Seq.head = Damaged then 0L
                        else
                            calculateVariantsNumber { States = line.States |> List.skip (count + 1); Numbers  = line.Numbers.Tail }
                else
                    0L
            let result = skipped + notSkipped    
            cache.Add(line, result)
            result
     

type Solution() =
    interface ISolution with
        override this.Input = createInput 12
        
        override this.SolvePart1(input) =
            let parsed = parseInput input
            let sum =
                parsed
                |> Seq.map calculateVariantsNumber
                |> Seq.sum
            printfn $"Sum is {sum}"
                                  
        override this.SolvePart2(input) =
            let parsed = parseInput input
            let sum =
                parsed
                |> Seq.map (
                    fun line ->
                        let newStates =
                            List.replicate 5 (List.ofSeq line.States)
                            |> List.intersperse (List.singleton Unknown)
                            |> List.collect id
                        let newNumbers =
                            List.replicate 5 (List.ofSeq line.Numbers)
                            |> List.collect id
                        { Numbers  = newNumbers; States = newStates }
                    )
                |> Seq.toArray
                |> Seq.map calculateVariantsNumber
                |> Seq.sum
            printfn $"Sum is {sum}"
