module AdventOfCode2023.Solutions.Days.Day10

open AdventOfCode2023.Inputs
open AdventOfCode2023.Solutions.Common
open AdventOfCode2023.Solutions.Utils

type PipeDirection = BottomTop | LeftRight | TopLeft | TopRight | BottomLeft | BottomRight
type Position = Pipe of PipeDirection | Ground | Start
type Coordinates = { X: int; Y: int; }
type Point = { Coordinates: Coordinates; Position: Position }

let isValidPoint (points: Point array array) (p: Coordinates) =
    p.Y >= 0 && p.Y < points.Length && p.X >= 0 && p.X < points[0].Length
    
let getNeighbours (point: Point) =
    let c = point.Coordinates
    match point.Position with
    | Start -> None
    | Ground -> None
    | Pipe BottomTop -> Some ({ c with Y = c.Y + 1 }, { c with Y = c.Y - 1 })
    | Pipe TopLeft -> Some ({ c with X = c.X - 1 }, { c with Y = c.Y - 1 })
    | Pipe TopRight -> Some ({ c with X = c.X + 1 }, { c with Y = c.Y - 1 })
    | Pipe LeftRight -> Some ({ c with X = c.X + 1 }, { c with X = c.X - 1 })
    | Pipe BottomLeft -> Some ({ c with X = c.X - 1 }, { c with Y = c.Y + 1 })
    | Pipe BottomRight -> Some ({ c with X = c.X + 1 }, { c with Y = c.Y + 1 })
    
let getLoop(input: Point array array) =
    let startPoint =
        input
        |> Seq.collect id
        |> Seq.find (fun pt -> pt.Position = Start)
    let start = startPoint.Coordinates
    let adjacent = [
        { start with X = start.X + 1 }
        { start with X = start.X - 1 }
        { start with Y = start.Y + 1 }
        { start with Y = start.Y - 1 }
    ]
    let getPoint (c:Coordinates) = input[c.Y][c.X]
    let isValid = isValidPoint input
    
    let neighbours =
        adjacent
        |> Seq.filter isValid
        |> Seq.map getPoint
        |> Seq.filter (
            fun p ->
                p
                |> getNeighbours
                |> Option.map (
                    fun (first, second) -> first = start || second = start)
                |> Option.defaultValue false)
        |> Seq.toList
    let neighbour = Seq.head neighbours
    let neighbourCoords =
        neighbours
        |> Seq.map _.Coordinates
        |> Set.ofSeq
    let startPosition =
        [BottomTop; LeftRight; TopLeft; TopRight; BottomLeft; BottomRight]
        |> Seq.map (fun dir -> { startPoint with Position = Pipe dir })
        |> Seq.choose (
            fun pt ->
                pt
                |> getNeighbours
                |> Option.map (fun nb -> (pt, nb)))
        |> Seq.filter (
            fun (_, (first, second)) ->
                neighbourCoords.Contains(first) && neighbourCoords.Contains(second))
        |> Seq.map fst
        |> Seq.map _.Position
        |> Seq.head
        
    seq {
        yield { startPoint with Position = startPosition }
        let mutable previous = startPoint
        let mutable current = neighbour
        while current <> startPoint do
            yield current
            current
            |> getNeighbours
            |> Option.iter (
                fun nbs ->
                    let newPrevious, newCurrent =
                        match nbs with
                        | n, _ when n = previous.Coordinates -> (current, getPoint(snd nbs)) 
                        | _ -> (current, getPoint(fst nbs))
                    previous <- newPrevious
                    current <- newCurrent)
    }
    |> Seq.toList

let getPosition(p: char) =
    match p with
    | '.' -> Ground
    | 'S' -> Start
    | '|' -> Pipe BottomTop
    | '-' -> Pipe LeftRight
    | 'L' -> Pipe TopRight
    | 'J' -> Pipe TopLeft
    | '7' -> Pipe BottomLeft
    | 'F' -> Pipe BottomRight
    | _   -> failwith $"Unknown position {p}"

let ParseInput (input: string) =
    input
    |> getLines
    |> Seq.mapi (
        fun y line ->
            line.ToCharArray()
            |> Seq.mapi (fun x ch -> { Coordinates = {X = x; Y = y}; Position = getPosition(ch) })
            |> Seq.toArray)
    |> Seq.toArray

type Solution() =
    interface ISolution with
        override this.Input = createCustomInput 10 (fun (input: Input) -> {
            new IInputWrapper with
                member x.GetPart01(real: bool) =
                    if real then input.RealData else input.GetData "part1-test"
                    
                member x.GetPart02(real: bool) =
                    if real then input.RealData else input.GetData "part2-test"
        })
        
        override this.SolvePart1(input) =
            let parsed = ParseInput input
            let loop = getLoop parsed
            let longest = (Seq.length loop + 1) / 2
            printfn $"Longest is {longest}"
                                  
        override this.SolvePart2(input) =
            let parsed = ParseInput input
            let loop = getLoop parsed
            let freePoints =
                parsed
                |> Seq.collect id
                |> Set.ofSeq
                |> fun pts ->
                    loop
                    |> Set.ofSeq 
                    |> Set.difference pts
            let horizontalRight =
                loop
                |> Seq.filter (
                    fun pt ->
                        match pt.Position with
                        | Pipe LeftRight | Pipe TopRight | Pipe BottomRight -> true
                        | _ -> false)
                |> Seq.map _.Coordinates
                |> Set.ofSeq
            let maxY =
                parsed
                |> Seq.collect id
                |> Seq.map _.Coordinates.Y
                |> Seq.max
            let getPoint (c:Coordinates) = parsed[c.Y][c.X]
            let opened =
                freePoints
                |> Seq.map _.Coordinates
                |> Seq.filter (
                    fun pt ->
                        seq { pt.Y+1..maxY }
                        |> Seq.map (fun y -> {pt with Y = y})
                        |> Seq.map getPoint
                        |> Seq.map _.Coordinates
                        |> Seq.filter horizontalRight.Contains
                        |> Seq.length
                        |> fun x -> x % 2 = 1)
                |> Seq.length
            printfn $"Opened count {opened}"    
