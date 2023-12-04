module AdventOfCode2023.Solutions.Common

open System
open AdventOfCode2023.Inputs

[<AbstractClass>]
type DayBase(day: int) =
    let input = Input(day)
    
    abstract member SolvePart1: string -> unit
    abstract member SolvePart2: string -> unit
        
    abstract member GetPart01: bool -> string
    abstract member GetPart02: bool -> string
    
    member this.Input with get() = input
    
    member this.Execute() =
        System.Console.WriteLine("Test data")
        this.SolvePart1(this.GetPart01(false))
        this.SolvePart2(this.GetPart02(false))
        System.Console.WriteLine("Real data")
        this.SolvePart1(this.GetPart01(true))
        this.SolvePart2(this.GetPart02(true))
    
    default x.GetPart01(real: bool) =
        match real with
        | false -> input.TestData
        | true -> input.RealData
        
    default x.GetPart02(real: bool) =
        match real with
        | false -> input.TestData
        | true -> input.RealData

let SwapParts(a, b) = (b, a)

let GetLines(input: string) = input.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
