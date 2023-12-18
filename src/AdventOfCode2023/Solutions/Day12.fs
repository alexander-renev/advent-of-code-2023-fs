module AdventOfCode2023.Solutions.Days.Day12

open System
open System.Collections.Generic
open FSharpPlus
open AdventOfCode2023.Solutions.Common
open AdventOfCode2023.Solutions.Utils

type State = Operational | Damaged | Unknown

type Line = { States: LinkedList<State>; Numbers: LinkedList<int> }

let printList (list: 'a seq) = $"""[{String.Join("; ", list)}]"""

let toLinkedList(source: 'a seq) =
    let result = LinkedList<_>()
    source
    |> Seq.iter (fun x -> result.AddLast x |> ignore)
    result
    
let remaining(current: LinkedListNode<'a>) =
    if current = null then
        Seq.empty
    else
        seq {
            let mutable item = current.Next
            while item <> null do
                yield item
                item <- item.Next
        }

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
                |> toLinkedList
            let numbers =
                parts[1].Split(',')
                |> Seq.map Int32.Parse
                |> toLinkedList
            { States = springs; Numbers = numbers }
        )
    |> Seq.toList
    
let tryMove(position: LinkedListNode<State>) (count: int): LinkedListNode<State> option =
    if count = 1 then
        if position.Next <> null && position.Next.Value <> Damaged then
            Some position.Next
        elif position.Next = null then
            Some position
        else
            None
    else
        let nextValues =
            remaining position
            |> Seq.toList
        if Seq.length nextValues < count - 1 then
            None
        elif
            Seq.length nextValues = count - 1 then
                if nextValues |> Seq.forall (fun v -> v.Value <> Operational) then
                    Some <| Seq.last nextValues
                else None
        elif
            Seq.length nextValues = count then
                let nextMatch =
                    nextValues |> Seq.take (count - 1) |> Seq.forall (fun v -> v.Value <> Operational)
                let lastMatch = Seq.last(nextValues).Value <> Damaged
                if nextMatch && lastMatch then
                    Some <| Seq.last nextValues
                else
                    None
        elif 
            nextValues |> Seq.take (count - 1) |> Seq.forall (fun v -> v.Value <> Operational)
            then
                let after =
                    nextValues
                    |> Seq.skip (count - 1)
                    |> Seq.head
                if after.Value = Damaged then
                    None
                else
                    Some after
        else
            None
                    
let rec calculateVariantsRec (linePosition: LinkedListNode<State>) (numberPosition: LinkedListNode<int>): int =
    if
        ((remaining linePosition) |> Seq.filter (fun p -> p.Value <> Operational) |> Seq.length) <
        ((remaining numberPosition) |> Seq.map _.Value |> Seq.sum) then
        0
    elif numberPosition = null then
        if linePosition = null
            then 1
        elif linePosition.Value <> Damaged && (remaining linePosition |> Seq.forall (fun p -> p.Value <> Damaged))
            then 1
        else
            0
    elif linePosition = null then
        0
    elif linePosition.Value = Operational then
        if linePosition.Next = null then 0
        else calculateVariantsRec linePosition.Next numberPosition
    elif linePosition.Value = Damaged then
        // next n values must be damaged, and then should be end or not damaged
        let damagedCount = numberPosition.Value
        match tryMove linePosition damagedCount with
        | Some newPosition when newPosition.Next <> null -> calculateVariantsRec newPosition.Next numberPosition.Next
        | Some _ -> calculateVariantsRec null numberPosition.Next
        | _ -> 0
    else
        linePosition.Value <- Damaged
        let ifDamaged = calculateVariantsRec linePosition numberPosition
        linePosition.Value <- Operational
        let ifOperational = calculateVariantsRec linePosition numberPosition
        linePosition.Value <- Unknown
        ifDamaged + ifOperational
let calculateVariantsNumber (line: Line) =
    let result = calculateVariantsRec line.States.First line.Numbers.First
    printf "."
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
                            |> toLinkedList
                        let newNumbers =
                            List.replicate 5 (List.ofSeq line.Numbers)
                            |> List.collect id
                            |> toLinkedList
                        { Numbers  = newNumbers; States = newStates }
                    )
                |> Seq.toArray
                |> Array.Parallel.map calculateVariantsNumber
                |> Seq.sum
            printfn $"Sum is {sum}"
            ()
