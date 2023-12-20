module AdventOfCode2023.Solutions.Days.Day11

open AdventOfCode2023.Solutions.Common
open AdventOfCode2023.Solutions.Utils

type Point = {X: int64; Y: int64;}
type Cell = Galaxy | Space of Point

let toArray source =
    source
    |> Seq.map Seq.toArray
    |> Seq.toArray

let expand (count: int64) (expander: int64 -> Point -> Cell) (source: Cell array array) =
    source
    |> Seq.map (
        fun line ->
            if line |> Seq.forall (fun cell -> cell <> Galaxy) then
                line
                |> Seq.map (
                    fun cell ->
                        match cell with
                        | Space point -> expander count point
                        | Galaxy -> failwith "Cannot be galaxy here")
                |> Seq.toArray
            else
                line)
    |> Seq.toArray

let horizontalExpander (count: int64) (point: Point) =
    Space { point with X = count}

let verticalExpander (count: int64) (point: Point) =
    Space { point with Y = count}
    
let calculate (source: Cell array array) (count: int64) =
    let expanded =
        source
        |> expand count verticalExpander
        |> Seq.transpose
        |> toArray
        |> expand count horizontalExpander
        |> Seq.transpose
        |> toArray
        
    let galaxies =
        seq {
            for y, line in expanded |> Seq.indexed do
                for x, ch in line |> Seq.indexed do
                    yield (x, y, ch)
        }
        |> Seq.filter (fun (_, _, ch) -> ch = Galaxy)
        |> Seq.map (fun (x, y, _) -> (x, y))
        |> Seq.toList
        
    let total =
        // Not checking for duplicate pairs, just divide total by 2
        galaxies
        |> Seq.allPairs galaxies
        |> Seq.filter (fun (g1, g2) -> g1 <> g2)
        |> Seq.map (
            fun ((x1, y1), (x2, y2)) ->
                let deltaX = if x2 > x1 then 1 else -1
                let deltaY = if y2 > y1 then 1 else -1
                seq {
                    for x in (x1 + deltaX) .. deltaX .. x2 do
                        let delta =
                            match expanded[y1][x] with
                            | Galaxy -> int64 1
                            | Space point -> point.X
                        yield delta
                    for y in (y1 + deltaY) .. deltaY .. y2 do
                        let delta =
                            match expanded[y][x2] with
                            | Galaxy -> int64 1
                            | Space point -> point.Y
                        yield delta
                }
                |> Seq.sum)
        |> Seq.sum
    total / 2L
    
let parseInput(input: string) =
    input
    |> getLines
    |> Seq.map (
        fun line ->
            line.ToCharArray()
            |> Seq.map (
                fun ch ->
                    if ch = '#' then
                        Galaxy
                    else
                        Space { X = 1L; Y = 1L })
            |> Seq.toArray)
    |> Seq.toArray

type Solution() =
    interface ISolution with
        override this.Input = createInput 11
        
        override this.SolvePart1(input) =
            let source = parseInput input
            let total = calculate source 2L
            printfn $"Sum is {total}"
                                  
        override this.SolvePart2(input) =
            let source = parseInput input
            let total = calculate source 1_000_000L
            printfn $"Sum is {total}"
