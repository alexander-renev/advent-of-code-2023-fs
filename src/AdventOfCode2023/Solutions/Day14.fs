module AdventOfCode2023.Solutions.Days.Day14

open System
open System.Collections.Generic
open System.Text
open AdventOfCode2023.Solutions.Common
open AdventOfCode2023.Solutions.Utils

type Position = Empty | Cube | Round

type Direction = North | South | East | West

type Point = { X: int; Y: int; }

let parsePosition =
    function
        | 'O' -> Round
        | '#' -> Cube
        | '.' -> Empty
        | x -> failwith $"Unknown position {x}"
        
let printPosition pos =
    match pos with
    | Round -> 'O'
    | Cube -> '#'
    | Empty -> '.'

let parseInput (input: string) =
    let result = Dictionary<_, _>()
    let lines = getLines input
    lines
    |> Seq.iteri (
        fun y line ->
            line
            |> Seq.iteri (
                fun x ch ->
                    result[{ X = x; Y = y }] <- parsePosition ch)
        )
    result
    
let getWidth (input: Dictionary<Point, Position>) =
    input.Keys
    |> Seq.map _.X
    |> Seq.max
    
let getHeight (input: Dictionary<Point, Position>) =
    input.Keys
    |> Seq.map _.Y
    |> Seq.max

let print (input: Dictionary<Point, Position>) =
    let sb = StringBuilder()
    let width = getWidth input
    let height = getHeight input
    for y in 0 .. height do
        for x in 0 .. width do
            sb.Append (printPosition input[{ X = x; Y = y }]) |> ignore
        sb.AppendLine() |> ignore
    sb.ToString()
    
let getMovePoints (direction: Direction) (input: Dictionary<Point, Position>) =
    let width = getWidth input
    let height = getHeight input
    if direction = North || direction = South then
        seq { 0 .. width } // x
        |> Seq.map (
            fun x ->
                let ys = if direction = North then seq { 0 .. height } else seq { height .. -1 .. 0 }
                ys
                |> Seq.map (fun y -> { X = x; Y = y })
                |> Seq.toList
            )
        |> Seq.toList
    else
        seq { 0 .. height } // y
        |> Seq.map (
            fun y ->
                let xs = if direction = West then seq { 0 .. width } else seq { width .. -1 .. 0 }
                xs
                |> Seq.map (fun x -> { X = x; Y = y })
                |> Seq.toList
            )
        |> Seq.toList
    
let move (direction:Direction) (input: Dictionary<Point, Position>) =
    let movePoints = getMovePoints direction input
    
    movePoints
    |> Seq.iter (
        fun points ->
        let mutable moved = true
        while moved do
            let positionToMove =
                points
                |> Seq.pairwise
                |> Seq.filter (
                    fun (prev, pos) ->
                        input[pos] = Round && input[prev] = Empty)
                |> Seq.tryHead
            positionToMove
            |> Option.iter (
                fun (prev, pos) ->
                    input[prev] <- Round
                    input[pos] <- Empty
                )
            moved <- Option.isSome positionToMove
        )
    input
    
let moveCycle (input: Dictionary<Point, Position>) =
    input
    |> move North
    |> move West
    |> move South
    |> move East
    |> ignore
    
let getWeight (input: Dictionary<Point, Position>) =
    let width = getWidth input
    let height = getHeight input
    
    seq { 0 .. width }
    |> Seq.allPairs (seq { 0 .. height })
    |> Seq.map (fun (y, x) -> { X = x; Y = y })
    |> Seq.filter (fun pt -> input[pt] = Round)
    |> Seq.map (fun pt -> height + 1 - pt.Y)
    |> Seq.sum

type Solution() =
    interface ISolution with
        override this.Input = createInput 14
        
        override this.SolvePart1(input) =
            let parsed = parseInput input
            let result =
                parsed
                |> move North
                |> getWeight
            printfn $"Total load is {result}"
                                  
        override this.SolvePart2(input) =
            let parsed = parseInput input
            let cache = Dictionary<string, int>()
            let steps = 1_000_000_000
            
            0
            |> Seq.unfold (
                fun state ->
                    moveCycle parsed
                    let description = print parsed
                    let newState =
                        if cache.TryAdd(description, state) = false then
                            let period = state - cache[description]
                            state + (steps - state) / period * period + 1
                        else
                            state + 1
                    Some (newState, newState)
                )
            |> Seq.takeWhile (fun s -> s < steps)
            |> Seq.iter ignore

            let weight =
                parsed
                |> getWeight
            printfn $"Total load is {weight}"
