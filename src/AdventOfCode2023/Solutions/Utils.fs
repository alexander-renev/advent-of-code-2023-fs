module AdventOfCode2023.Solutions.Utils

open System
open FParsec

let swapParts(a, b) = (b, a)

let getLines(input: string) = input.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)

let whitespace1 x = many1(pchar ' ') x

let whitespace x = many(pchar ' ') x

let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        printfn $"%A{stream.Position}: Entering %s{label}"
        let reply = p stream
        printfn $"%A{stream.Position}: Leaving %s{label} (%A{reply.Status})"
        reply
