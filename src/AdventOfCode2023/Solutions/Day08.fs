module AdventOfCode2023.Solutions.Days.Day08

open System
open System.Collections.Generic
open System.Numerics
open AdventOfCode2023.Inputs
open AdventOfCode2023.Solutions.Common
open FParsec


type Command = GoLeft | GoRight

type Node = { Left: string; Right: string; }

type Description = { Positions: int64 list; Period: bigint }

type Solution() =
    interface ISolution with
        override this.Input = createCustomInput 8 (fun (input: Input) -> {
            new IInputWrapper with
                member x.GetPart01(real: bool) =
                    if real then input.RealData else input.GetData "part1-test"
                    
                member x.GetPart02(real: bool) =
                    if real then input.RealData else input.GetData "part2-test"
        })
        
        override this.SolvePart1(input) =
            let commands, nodes = this.ParseInput input
            let mutable position = "AAA"
            let count =
                this.MakeInstructions commands
                |> Seq.map(
                    fun instruction ->
                        let result = 
                            match snd instruction with
                            | GoLeft -> nodes[position].Left
                            | GoRight -> nodes[position].Right
                        position <- result
                        result
                    )
                |> Seq.takeWhile (fun p -> not <| p.Equals "ZZZ")
                |> Seq.length
            printfn $"Steps count is {count + 1}"
                                  
        override this.SolvePart2(input) =
            let commands, nodes = this.ParseInput input
            let mutable positions =
                nodes
                |> Seq.map _.Key
                |> Seq.filter (fun s -> s.EndsWith "A")
                |> Seq.toList
            
            let descriptions = positions |> Seq.map (fun pos -> this.DescribeInput pos commands nodes) |> Seq.toList
            // For test data we always get Period == Position, so we just need to find least common multiple of periods
            // LCM(a, b) = a * b / GCD(a, b)
            let result =
                descriptions
                |> Seq.map _.Period
                |> Seq.reduce (fun a b -> a * b / BigInteger.GreatestCommonDivisor(a, b))
            printfn $"Steps count is {result}"
    
    member private this.DescribeInput (start: string) (commands: Command list) (map: IDictionary<String, Node>) =
        let visited = HashSet<_>()
        let mutable position = start
        visited.Add (position, -1) |> ignore
        let result = ResizeArray<_>()
        
        this.MakeInstructions commands
        |> Seq.mapi (
            fun index (commandIndex, command) ->
                position <-
                    match command with
                    | GoLeft -> map[position].Left
                    | GoRight -> map[position].Right
                result.Add (position, index, commandIndex)
                visited.Add (position, commandIndex)
            )
        |> Seq.takeWhile id
        |> Seq.reduce (fun a _ -> a)
        |> ignore
        
        let duplicates =
            result
            |> Seq.groupBy (fun (position, _, commandIndex) -> (position, commandIndex))
            |> Seq.filter (fun x -> Seq.length(snd x) > 1)
            |> Seq.head
            |> snd
            |> Seq.toList
            
        let period =
            duplicates
            |> Seq.rev
            |> Seq.map(fun (_, index, _) -> index)
            |> Seq.reduce (-)
            
        let positions =
            result
            |> Seq.filter (fun (position, _, _) -> position.EndsWith "Z")
            |> Seq.map (fun (_, index, _) -> int64(index + 1))
            |> Seq.toList
        
        { Positions = positions ; Period = bigint(period) }
    
    member private this.MakeInstructions(instructions: Command list) =
        seq {
            while true do
                yield! instructions |> Seq.mapi (fun instruction index -> (instruction, index))
        }
        
    member private this.ParseInput(input: string) =
        let instructionParser = choice [
            pstring "R" >>% Command.GoRight
            pstring "L" >>% Command.GoLeft
        ]
        let instructionsParser = many1 instructionParser
        let itemParser =
            many1 <| satisfy Char.IsLetterOrDigit
            |>> (fun chars -> String(List.toArray(chars)))
        let nodeParser =
            itemParser
            .>> pstring " = ("
            .>>. itemParser
            .>> pstring ", "
            .>>. itemParser
            .>> pstring ")"
            |>> (fun ((from, toLeft), toRight) -> (from, {Left = toLeft; Right = toRight}))
        let nodesParser =
            sepEndBy nodeParser newline
            |>> dict
        let inputParser =
            instructionsParser
            .>> newline
            .>> newline
            .>>. nodesParser
        match run inputParser input with
        | Success(result, _, _) -> result
        | Failure(errorMsg, _, _) -> failwith errorMsg
