module AdventOfCode2023.Solutions.Days.Day04

open System
open System.Collections.Generic
open AdventOfCode2023.Solutions.Common
open FParsec
open AdventOfCode2023.Solutions.Utils

type Card = { Number: int; WinningNumbers: Set<int>; ExistingNumbers: Set<int>  }

type Solution() =
    interface ISolution with
        override this.Input = createInput 4
        
        override this.SolvePart1(input) =
            let cards = this.parseInput input
            let sum =
                cards
                |> Seq.map (
                    fun card ->
                        match
                            card.WinningNumbers
                            |> Set.intersect card.ExistingNumbers
                            |> Seq.length
                        with
                        | 0 -> int64 0
                        | x -> Seq.replicate (x-1) (int64 2) |> Seq.fold (*) (int64 1)
                    )
                |> Seq.sum
            printfn $"Sum is {sum}"
                                  
        override this.SolvePart2(input) =
            let cards = this.parseInput input
            let cardsCount = Dictionary<int, int>(
                cards
                |> Seq.map(fun card -> (card.Number, 1))
                |> dict)
            
            cards
            |> Seq.iter (
                fun card ->
                    let count =
                        card.WinningNumbers
                        |> Set.intersect card.ExistingNumbers
                        |> Seq.length
                        
                    if count > 0 then
                        seq { card.Number + 1 .. card.Number + count }
                        |> Seq.iter (
                            fun id ->
                                let found, currentValue = cardsCount.TryGetValue(id)
                                if found then cardsCount[id] <- currentValue + cardsCount[card.Number]))
            
            let sum =
                cardsCount.Values
                |> Seq.sum
            printfn $"Sum is {sum}"
    
    member private x.parseInput(input: string) =
        let numbersParser = sepEndBy1 pint32 whitespace1
        let cardParser =
            pstring "Card"
            >>. spaces1
            >>. pint32
            .>> pstring ":"
            .>> spaces1
            .>>. numbersParser
            .>> pstring "|"
            .>> spaces1
            .>>. numbersParser
            |>> (fun ((number, winning), existing) -> {
                Number = number
                WinningNumbers = Set.ofList(winning)
                ExistingNumbers = Set.ofList(existing)
            })
        let cardsParser = sepEndBy1 cardParser newline
        match run cardsParser input with
        | Success(result, _, _) -> result
        | Failure(errorMsg, _, _) -> failwith errorMsg
