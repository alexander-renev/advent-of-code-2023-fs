module AdventOfCode2023.Solutions.Days.Day16

open System
open System.Collections.Generic
open AdventOfCode2023.Solutions.Common
open AdventOfCode2023.Solutions.Utils

[<Flags>]    
type Direction =
    | None = 0
    | Up = 1
    | Down = 2
    | Left = 4
    | Right = 8
    
let directions = [Direction.Up; Direction.Down; Direction.Left; Direction.Right]

let getCount (direction: Direction) =
    directions
    |> Seq.filter (fun d -> d &&& direction = d)
    |> Seq.length
    |> _.ToString().ToCharArray()
    |> Seq.head
    
type Point = { X: int; Y: int; }

type Mirrors = IDictionary<Point, Char>
type Visited = Dictionary<Point, Direction>
type Light = Point * Direction

let print (mirrors: Mirrors) (visited: Visited) =
    let maxX = mirrors.Keys |> Seq.map (_.X) |> Seq.max
    let maxY = mirrors.Keys |> Seq.map (_.Y) |> Seq.max
    
    for y in seq { 0 .. maxY } do
        for x in seq { 0 .. maxX } do
            let point = { X = x; Y = y }
            let ch =
                if mirrors[point] <> '.' then
                    mirrors[point]
                elif visited[point] = Direction.None then
                    mirrors[point]
                else
                    match visited[point] with
                    | Direction.Down -> 'V'
                    | Direction.Up -> '^'
                    | Direction.Left -> '<'
                    | Direction.Right -> '>'
                    | x -> getCount x
            printf $"{ch}"
        Console.WriteLine()

let moveNext (light: Light) =
    let position, direction = light
    let newPosition =
        match direction with
        | Direction.Up -> { position with Y = position.Y - 1 }
        | Direction.Down -> { position with Y = position.Y + 1 }
        | Direction.Left -> { position with X = position.X - 1 }
        | Direction.Right -> { position with X = position.X + 1 }
        | _ -> failwith "Light direction cannot be mixed"
    newPosition, direction

let parseInput (input: string) =
    seq {
        for y, line in input |> getLines |> Seq.indexed do
            for x, ch in line |> Seq.indexed do
                yield ({ X = x; Y = y }, ch)
    }
    |> dict
    
let getEnergizedCount (parsed: Mirrors) (start: Light) =
    let visited = Visited()
    for pair in parsed do
        visited.Add(pair.Key, Direction.None)
    let queue = Queue<_>()
    queue.Enqueue start
    while queue.Count > 0 do
        let position, direction = queue.Dequeue()
        if visited.ContainsKey(position) = false || visited[position] &&& direction = direction then
            ()
        else
            visited[position] <- visited[position] ||| direction
            let nextDirections =
                match parsed[position] with
                | '.'
                     -> [direction]
                | '|' when direction = Direction.Up || direction = Direction.Down
                     -> [direction]
                | '-' when direction = Direction.Left || direction = Direction.Right
                     -> [direction]
                | '/' ->
                    match direction with
                    | Direction.Down -> [Direction.Left]
                    | Direction.Up -> [Direction.Right]
                    | Direction.Left -> [Direction.Down]
                    | Direction.Right -> [Direction.Up]
                    | _ -> failwith "Cannot be mixed direction"
                | '\\' ->
                    match direction with
                    | Direction.Down -> [Direction.Right]
                    | Direction.Up -> [Direction.Left]
                    | Direction.Left -> [Direction.Up]
                    | Direction.Right -> [Direction.Down]
                    | _ -> failwith "Cannot be mixed direction"
                | '|' ->
                    [Direction.Up; Direction.Down]
                | '-' ->
                    [Direction.Left; Direction.Right]
                | _ -> failwith $"Unknown char {parsed[position]}"
            for newDirection in nextDirections do
                queue.Enqueue <| moveNext (position, newDirection)
                    
    let energized =
        visited.Values
        |> Seq.filter (fun v -> v <> Direction.None)
        |> Seq.length
    energized

type Solution() =
    interface ISolution with
        override this.Input = createInput 16
        
        override this.SolvePart1(input) =
            let parsed = input |> parseInput
            let energized = ({ X = 0; Y = 0 }, Direction.Right) |> getEnergizedCount parsed
            printfn $"{energized} energized tiles"
            ()
            
        override this.SolvePart2(input) =
            let parsed = input |> parseInput
            let getEnergized = parsed |> getEnergizedCount
            let maxX = parsed.Keys |> Seq.map (_.X) |> Seq.max
            let maxY = parsed.Keys |> Seq.map (_.Y) |> Seq.max
            
            let vertical = seq {
                for x in seq { 0 .. maxX } do
                    yield! [ { X = x; Y = 0 }, Direction.Down; { X = x; Y = maxY }, Direction.Up ]
            }
            let horizontal = seq {
                for y in seq { 0 .. maxY } do
                    yield! [ { X = 0; Y = y }, Direction.Right; { X = maxX; Y = y }, Direction.Left ] 
            }
            let maxEnergized =
                vertical
                |> Seq.append horizontal
                |> Seq.map getEnergized
                |> Seq.max
            printfn $"Max energy is {maxEnergized}"
