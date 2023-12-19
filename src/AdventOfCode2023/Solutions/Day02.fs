module AdventOfCode2023.Solutions.Days.Day02

open System.Collections.Generic
open AdventOfCode2023.Solutions.Common
open AdventOfCode2023.Solutions.Utils
open FParsec

[<Struct>]
type Color = Blue | Red | Green

[<Struct>]
type Card = { Number: int; Combinations: IDictionary<Color, int> list }

let suitsForPart1(card: Card) =
    let existing = dict [ (Color.Red, 12); (Color.Green, 13); (Color.Blue, 14) ]
    card.Combinations
    |> Seq.forall (Seq.forall (fun pair -> existing[pair.Key] >= pair.Value))

let processCardForPart2(card: Card) =
    let colors = [Color.Red; Color.Green; Color.Blue]
    colors
    |> Seq.map (
        fun color ->
            card.Combinations
            |> Seq.map (fun combination -> snd <| combination.TryGetValue(color))
            |> Seq.max)
    |> Seq.reduce (*)

let parseInput(input: string) =
    let colorParser = choice [
        pstring "blue" >>% Color.Blue
        pstring "red" >>% Color.Red
        pstring "green" >>% Color.Green
    ]
    let combinationParser =
        pint32
        .>> spaces
        .>>. colorParser
        |>> swapParts
    let combinationsParser = sepBy combinationParser (pstring ", ") |>> dict
    let gameParser =
        pstring "Game "
        >>. pint32
        .>> pstring ": "
        .>>. sepBy combinationsParser (pstring "; ")
        |>> fun (num, combinations) -> { Number = num; Combinations = combinations }
    let gamesParser = sepEndBy gameParser newline
    match run gamesParser input with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> failwith errorMsg

type Solution() =
    interface ISolution with
        override this.Input = createInput 2
        override this.SolvePart1(input) =
            let sum =
                parseInput input
                |> Seq.filter suitsForPart1
                |> Seq.map _.Number
                |> Seq.sum
            printfn $"Sum is {sum}"
                                  
        override this.SolvePart2(input) =
            let sum =
                parseInput input
                |> Seq.map processCardForPart2
                |> Seq.sum
            printfn $"Sum is {sum}"
