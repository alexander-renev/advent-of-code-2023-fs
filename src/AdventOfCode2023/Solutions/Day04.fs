﻿module AdventOfCode2023.Solutions.Days.Day04

open System.Collections.Generic
open AdventOfCode2023.Solutions.Common
open FParsec
open AdventOfCode2023.Solutions.Utils

type Card =
    { Number: int
      WinningNumbers: Set<int>
      ExistingNumbers: Set<int> }

let parseInput (input: string) =
    let numbersParser = sepEndBy1 pint32 whitespace1

    let cardParser =
        pstring "Card" >>. spaces1 >>. pint32 .>> pstring ":" .>> spaces1
        .>>. numbersParser
        .>> pstring "|"
        .>> spaces1
        .>>. numbersParser
        |>> (fun ((number, winning), existing) ->
            { Number = number
              WinningNumbers = Set.ofList (winning)
              ExistingNumbers = Set.ofList (existing) })

    let cardsParser = sepEndBy1 cardParser newline

    match run cardsParser input with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> failwith errorMsg

type Solution() =
    interface ISolution with
        override this.Input = createInput 4

        override this.SolvePart1(input) =
            let cards = parseInput input

            let sum =
                cards
                |> Seq.map (fun card ->
                    match card.WinningNumbers |> Set.intersect card.ExistingNumbers |> Seq.length with
                    | 0 -> 0L
                    | x -> 2L |> Seq.replicate (x - 1) |> Seq.reduce (*))
                |> Seq.sum

            printfn $"Sum is {sum}"

        override this.SolvePart2(input) =
            let cards = parseInput input

            let cardsCount =
                cards |> Seq.map (fun card -> (card.Number, 1) |> kvp) |> Dictionary<_, _>

            cards
            |> Seq.iter (fun card ->
                let count = card.WinningNumbers |> Set.intersect card.ExistingNumbers |> Seq.length

                let cards =
                    match count with
                    | x when x > 0 -> seq { card.Number + 1 .. card.Number + x }
                    | _ -> Seq.empty

                cards
                |> Seq.filter cardsCount.ContainsKey
                |> Seq.iter (fun id -> cardsCount[id] <- cardsCount[id] + cardsCount[card.Number]))

            let sum = cardsCount.Values |> Seq.sum
            printfn $"Sum is {sum}"
