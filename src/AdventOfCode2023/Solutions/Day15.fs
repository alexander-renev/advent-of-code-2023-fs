module AdventOfCode2023.Solutions.Days.Day15

open System
open System.Collections.Generic
open AdventOfCode2023.Solutions.Common
open AdventOfCode2023.Solutions.Utils

type Command = Remove of string | Replace of string * int

type Lens = { Label: string; Length: int;  }

type Boxes = IDictionary<int, LinkedList<Lens>>

let parseInput1 (input: string) = input.Split(',', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)

let print (boxes: Boxes) =
    seq { 0 .. 255 }
    |> Seq.filter (fun i -> boxes[i].Count > 0)
    |> Seq.iter (
        fun i ->
            let box = boxes[i]
            let elements =
                box
                |> Seq.map (fun item -> $"[{item.Label} {item.Length}]")
                |> fun x -> String.Join(" ", x)
            printfn $"Box {i}: {elements}")

let parseInput2 (input: string) =
    input
    |> parseInput1
    |> Seq.map (
        fun item ->
            let parts = item.Split('=', '-')
            if item.Contains '=' then
                Replace (parts[0], Int32.Parse(parts[1]))
            else
                Remove parts[0]
        )
    |> Seq.toList

let getHash (text: string) =
    (0, text.ToCharArray())
    ||> Seq.fold (
        fun state ch -> (state + int(ch)) * 17 % 256)

type Solution() =
    interface ISolution with
        override this.Input = createInput 15
        
        override this.SolvePart1(input) =
            let sum =
                input
                |> parseInput1
                |> Seq.map getHash
                |> Seq.sum
            printfn $"Sum is {sum}"
            
        override this.SolvePart2(input) =
            let parsed = parseInput2 input
            let boxes =
                seq { 0 .. 255 }
                |> Seq.map (
                    fun i -> (i, LinkedList<Lens>()))
                |> dict
            parsed
            |> Seq.iter (
                fun command ->
                    let label =
                        match command with
                        | Remove s -> s
                        | Replace (s, _) -> s
                    let box = boxes[getHash label]
                    match command with
                    | Remove s ->
                        box
                        |> iterateLinkedList
                        |> Seq.filter (fun item -> item.Value.Label = s)
                        |> Seq.toList
                        |> Seq.iter box.Remove
                    | Replace (s, power) ->
                        let existing =
                            box
                            |> iterateLinkedList
                            |> Seq.filter (fun item -> item.Value.Label = s)
                            |> Seq.tryHead
                        match existing with
                        | Some item -> item.ValueRef <- { Label  = s; Length = power; }
                        | None -> box.AddLast { Label  = s; Length = power; } |> ignore
                        ()
                )
            let lenses =
                boxes.Values
                |> Seq.collect id
                |> Seq.map _.Label
                |> Seq.distinct
                |> Seq.toList
            let power =
                lenses
                |> Seq.map (
                    fun lens ->
                        boxes
                        |> Seq.map (
                            fun box ->
                                box.Value
                                |> Seq.indexed
                                |> Seq.filter (fun (_, l) -> l.Label = lens)
                                |> Seq.map (fun (index, lens) -> (box.Key + 1) * (index + 1) * lens.Length)
                                |> Seq.sum
                            )
                        |> Seq.sum
                    )
                |> Seq.sum
            printfn $"Sum is {power}"
            ()
