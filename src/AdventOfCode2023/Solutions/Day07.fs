module AdventOfCode2023.Solutions.Days.Day07

open System
open System.Collections.Generic
open AdventOfCode2023.Solutions.Common
open AdventOfCode2023.Solutions.Utils

type InputHand = { Cards: string; Bet: int64 }

type Hand =
    { Cards: string
      Values: int array
      Bet: int64
      Power: int }

type Comparison =
    | Greater
    | Less

let cardValuesPart1 =
    "23456789TJQKA".ToCharArray()
    |> Seq.indexed
    |> Seq.map (fun (index, ch) -> (ch, index + 1))
    |> dict

let cardValuesPart2 =
    "J23456789TQKA".ToCharArray()
    |> Seq.indexed
    |> Seq.map (fun (index, ch) -> (ch, index + 1))
    |> dict

let getValues (valuesMap: IDictionary<char, int>) (text: string) =
    text.ToCharArray() |> Seq.map (fun ch -> valuesMap[ch]) |> Seq.toArray

let getValues1 = getValues cardValuesPart1
let getValues2 = getValues cardValuesPart2

let calculatePower (values: int array) : int =
    let counts =
        values
        |> Seq.groupBy id
        |> Seq.map (fun (_, grp) -> Seq.length grp)
        |> Seq.sortDescending
        |> Seq.toList

    match counts with
    | [ _ ] -> 6
    | [ 4; _ ] -> 5
    | [ 3; _ ] -> 4
    | [ 3; _; _ ] -> 3
    | [ 2; 2; _ ] -> 2
    | [ _; _; _; _ ] -> 1
    | [ _; _; _; _; _ ] -> 0
    | _ -> failwith $"Cannot describe values {values}"


let maximizeHand (hand: InputHand) : Hand =
    let hasJoker = hand.Cards.ToCharArray() |> Seq.contains 'J'

    if hasJoker = false then
        let values = getValues2 hand.Cards
        let power = calculatePower values

        { Cards = hand.Cards
          Bet = hand.Bet
          Power = power
          Values = values }
    else
        let maxChar =
            hand.Cards.ToCharArray()
            |> Seq.filter (fun ch -> ch <> 'J')
            |> Seq.groupBy id
            |> Seq.map (fun (key, values) -> (key, values |> Seq.length))
            |> Seq.sortByDescending snd
            |> Seq.tryHead
            |> Option.map fst
            |> Option.defaultValue 'T'

        let newCards = hand.Cards.Replace('J', maxChar)
        let newValues = getValues2 newCards
        let power = calculatePower newValues

        { Values = getValues2 hand.Cards
          Cards = hand.Cards
          Bet = hand.Bet
          Power = power }

let compareHands (hand1: Hand) (hand2: Hand) =
    match compare hand1.Power hand2.Power with
    | x when x <> 0 -> x
    | _ ->
        (hand1.Values, hand2.Values)
        ||> Array.zip
        |> Seq.choose (fun (h1, h2) ->
            match compare h1 h2 with
            | x when x <> 0 -> Some x
            | _ -> None)
        |> Seq.tryHead
        |> Option.defaultValue 0

let parseInput (input: string) : InputHand array =
    let lines = getLines input

    lines
    |> Seq.map (fun line ->
        match line.Split(' ') |> List.ofArray with
        | [ part1; part2 ] ->
            { Cards = part1
              Bet = Int64.Parse(part2) }
        | _ -> failwith "Expected list of 2 items")
    |> Seq.toArray

type Solution() =
    interface ISolution with
        override this.Input = createInput 7

        override this.SolvePart1(input) =
            let parsed = parseInput input

            let win =
                parsed
                |> Seq.map (fun hand ->
                    let values = getValues1 hand.Cards
                    let power = calculatePower values

                    { Cards = hand.Cards
                      Bet = hand.Bet
                      Values = values
                      Power = power })
                |> Seq.sortWith compareHands
                |> Seq.indexed
                |> Seq.sumBy (fun (index, hand) -> int64 (index + 1) * hand.Bet)

            printfn $"Total is {win}"

        override this.SolvePart2(input) =
            let parsed = parseInput input

            let win =
                parsed
                |> Seq.map maximizeHand
                |> Seq.sortWith compareHands
                |> Seq.indexed
                |> Seq.sumBy (fun (index, hand) -> int64 (index + 1) * hand.Bet)

            printfn $"Total is {win}"
