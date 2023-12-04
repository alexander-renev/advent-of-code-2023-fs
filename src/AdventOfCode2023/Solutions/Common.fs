module AdventOfCode2023.Solutions.Common

open System
open AdventOfCode2023.Inputs

    
type IInputWrapper =
    abstract member GetPart01: bool -> string
    
    abstract member GetPart02: bool -> string

type ISolution =
    abstract member SolvePart1: string -> unit
    abstract member SolvePart2: string -> unit
    
    abstract Input: IInputWrapper with get
    
type DefaultInputWrapper(input: Input) =
    interface IInputWrapper with
        member this.GetPart01(real: bool) =
            match real with
            | false -> input.TestData
            | true -> input.RealData
        member this.GetPart02(real: bool) =
            match real with
            | false -> input.TestData
            | true -> input.RealData
            
let createCustomInput (day: int) (factory: Input -> IInputWrapper) = factory <| Input(day)

let createInput (day: int) = createCustomInput day (fun input -> DefaultInputWrapper(input))

let executeSolution(solution: ISolution) =
    let input = solution.Input
    Console.WriteLine("Test data")
    solution.SolvePart1(solution.Input.GetPart01(false))
    solution.SolvePart2(solution.Input.GetPart02(false))
    Console.WriteLine("Real data")
    solution.SolvePart1(solution.Input.GetPart01(true))
    solution.SolvePart2(solution.Input.GetPart02(true))
