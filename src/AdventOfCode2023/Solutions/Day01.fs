namespace AdventOfCode2023.Solutions.Days

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open AdventOfCode2023.Solutions.Common

type Day01() =
    inherit DayBase(1)
    
    let digitsMap = [ ("one", 1); ("two", 2); ("three", 3)
                      ("four", 4); ("five", 5); ("six", 6)
                      ("seven", 7); ("eight", 8); ("nine", 9)]
    let numberMap = Dictionary<string, int>()
                           
    do
        digitsMap |> Seq.iter (fun (key, value) -> numberMap[key] <- value) 
        for n in 1 .. 9 do
            numberMap[n.ToString()] <- n
            
    let digitRegexp = Regex("\\d", RegexOptions.Compiled)
    
    override x.GetPart01(real: bool) =
        if real then x.Input.RealData else x.Input.GetData "part1-test"
        
    override x.GetPart02(real: bool) =
        if real then x.Input.RealData else x.Input.GetData "part2-test"

    override this.SolvePart1(input) =
        let sum =
            GetLines input
            |> Seq.map this.GetNumber
            |> Seq.sum
        printfn $"Sum is %i{sum}"
        
    override this.SolvePart2(input) =
        let sum =
            GetLines input
            |> Seq.map this.GetNumberOrText
            |> Seq.sum
        printfn $"Sum is {sum}"
        
    member private x.GetNumber(line: string) =
        let numbers =
            digitRegexp.Matches(line)
            |> Seq.map (fun x -> Int32.Parse(x.Value))
            |> Seq.toArray
        numbers[0] * 10 + numbers[^0]

    member private x.GetNumberOrText(line: string) =
        let validIndex = fun (_, index) -> index > -1
        
        let firstValue, _ =
            numberMap
            |> Seq.map (fun p -> (p, line.IndexOf(p.Key)))
            |> Seq.filter validIndex
            |> Seq.minBy snd
            
        let lastValue, _ =
            numberMap
            |> Seq.map (fun p -> (p, line.LastIndexOf(p.Key)))
            |> Seq.filter validIndex
            |> Seq.maxBy snd

        firstValue.Value * 10 + lastValue.Value
