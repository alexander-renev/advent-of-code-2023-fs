module AdventOfCode2023.Solutions.Days.Day05

open System
open System.Text.RegularExpressions
open AdventOfCode2023.Solutions.Common
open AdventOfCode2023.Solutions.Utils

type Range = { From: int64; To: int64 }

type RangeMap =
    { Source: int64
      Destination: int64
      Length: int64 }

type Input =
    { Seeds: int64 array
      Maps: RangeMap array array }

let numberRegex = Regex("\\d+", RegexOptions.Compiled)

let parseInput (input: string) : Input =
    let groups = input.Split(Environment.NewLine + Environment.NewLine)

    let seeds =
        numberRegex.Matches(groups[0])
        |> Seq.map (fun m -> Int64.Parse(m.ValueSpan))
        |> Seq.toArray

    let maps =
        groups
        |> Seq.skip 1
        |> Seq.map (fun grp ->
            grp
            |> getLines
            |> Seq.map numberRegex.Matches
            |> Seq.filter (fun matches -> matches.Count = 3)
            |> Seq.map (fun matches ->
                matches
                |> Seq.map (fun m -> Int64.Parse(m.ValueSpan))
                |> Seq.toArray
                |> fun ar ->
                    { Destination = ar[0]
                      Source = ar[1]
                      Length = ar[2] })
            |> Seq.toArray)
        |> Seq.toArray

    { Seeds = seeds; Maps = maps }


let mapSeed (input: Input) (seed: int64) : int64 =
    (seed, input.Maps)
    ||> Seq.fold (fun state row ->
        row
        |> Seq.filter (fun m -> state >= m.Source && state < m.Source + m.Length)
        |> Seq.map (fun m -> (state - m.Source) + m.Destination)
        |> Seq.tryHead
        |> Option.defaultValue state)

let mapRange (rng: Range) (mapping: RangeMap array) : Range array =
    Some rng.From
    |> Seq.unfold (
        Option.bind (fun position ->
            if position >= rng.To then
                None
            else
                let foundValue =
                    mapping
                    |> Seq.filter (fun r -> r.Source + r.Length > position + 1L)
                    |> Seq.sortBy _.Source
                    |> Seq.tryHead

                match foundValue with
                | None -> Some(List.singleton ({ From = position; To = rng.To }), None)
                | Some found ->
                    let delta = found.Destination - found.Source
                    // Start of range will be raw mapped
                    let start =
                        if found.Source > position then
                            Some { From = position; To = found.Source }
                        else
                            None
                    // Source range is fully mapped
                    let other, newPosition =
                        if found.Source + found.Length >= rng.To then
                            Some
                                { From = position + delta
                                  To = rng.To + delta },
                            None
                        else
                            // Map full destination range
                            Some
                                { From = position + delta
                                  To = found.Destination + found.Length },
                            Some(found.Source + found.Length)

                    let mapped =
                        seq {
                            yield start
                            yield other
                        }
                        |> Seq.choose id
                        |> Seq.toList

                    Some(mapped, newPosition))
    )
    |> Seq.collect id
    |> Seq.toArray

type Solution() =
    interface ISolution with
        override this.Input = createInput 5

        override this.SolvePart1(input) =
            let parsed = parseInput input
            let location = parsed.Seeds |> Seq.map (mapSeed parsed) |> Seq.min
            printfn $"Location number is {location}"

        override this.SolvePart2(input) =
            let parsed = parseInput input

            let result =
                parsed.Seeds
                |> Array.indexed
                |> Array.partition (fun (index, _) -> index % 2 = 0)
                ||> Array.zip
                |> Seq.map (fun ((_, first), (_, second)) -> { From = first; To = first + second })
                |> Seq.map (fun range ->
                    (List.singleton (range), parsed.Maps)
                    ||> Seq.fold (fun state m ->
                        state |> Seq.map (fun item -> mapRange item m) |> Seq.collect id |> Seq.toList))
                |> Seq.collect id
                |> Seq.map _.From
                |> Seq.min

            printfn $"Location number is {result}"
