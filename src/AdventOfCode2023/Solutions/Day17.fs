module AdventOfCode2023.Solutions.Days.Day17

open System
open System.Collections.Generic
open AdventOfCode2023.Solutions.Common
open AdventOfCode2023.Solutions.Utils

type Direction =
    | Up
    | Down
    | Left
    | Right

let directions = [ Direction.Up; Direction.Down; Direction.Left; Direction.Right ]

type Point = { X: int; Y: int }

let move (point: Point) (direction: Direction) =
    match direction with
    | Up -> { point with Y = point.Y - 1 }
    | Down -> { point with Y = point.Y + 1 }
    | Left -> { point with X = point.X - 1 }
    | Right -> { point with X = point.X + 1 }

let parseInput (input: string) =
    let lines = getLines input

    seq {
        for y, line in Seq.indexed (lines) do
            for x, ch in Seq.indexed (line) do
                yield ({ X = x; Y = y }, Int32.Parse(ch.ToString()))
    }
    |> dict

type Solution() =
    interface ISolution with
        override this.Input = createInput 17

        override this.SolvePart1(input) =
            let inputData = parseInput input
            let maxX = inputData.Keys |> Seq.map (_.X) |> Seq.max
            let maxY = inputData.Keys |> Seq.map (_.Y) |> Seq.max
            let zero = { X = 0; Y = 0 }

            let visited =
                seq {
                    for pt in inputData.Keys do
                        for direction in directions do
                            yield ((pt, direction), None) |> kvp
                }
                |> Dictionary<_, _>

            for direction in directions do
                visited[(zero, direction)] <- Some 0

            let queue = Queue<Point * Direction * int>()
            queue.Enqueue(zero, Direction.Right, 0)
            queue.Enqueue(zero, Direction.Down, 0)

            while queue.Count > 0 do
                let point, direction, moved = queue.Dequeue()

                let directions =
                    seq {
                        if direction = Up || direction = Down then
                            yield! [ Left; Right ]
                        else
                            yield! [ Up; Down ]

                        if moved < 3 then
                            yield direction
                    }

                for moveDirection in directions do
                    let nextPosition = move point moveDirection
                    let newMoved = if moveDirection = direction then moved + 1 else 1

                    if inputData.ContainsKey nextPosition then
                        let newWeight = visited[point, direction].Value + inputData[nextPosition]
                        let currentWeight = visited[nextPosition, moveDirection]

                        if Option.isNone (currentWeight) || currentWeight.Value > newWeight then
                            visited[(nextPosition, moveDirection)] <- Some newWeight
                            queue.Enqueue(nextPosition, moveDirection, newMoved)

            let bottomRight = { X = maxX; Y = maxY }

            let minimumLoss =
                directions
                |> Seq.map (fun direction -> visited[bottomRight, direction])
                |> Seq.choose id
                |> Seq.max
                |> fun x -> x + inputData[bottomRight]

            printfn $"Minimum loss is {minimumLoss}"

        override this.SolvePart2(input) = ()
