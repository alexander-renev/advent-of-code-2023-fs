module AdventOfCode2023.Solutions.Utils

open System
open System.Collections.Generic
open FParsec

let swapParts (a, b) = (b, a)

let getLines (input: string) =
    input.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)

let whitespace1 x = many1 (pchar ' ') x

let whitespace x = many (pchar ' ') x

let pairWith b a = (a, b)

let reversePairWith b a = (b, a)

let iterateLinkedList (lst: LinkedList<'a>) =
    lst.First
    |> Seq.unfold (fun node -> if node = null then None else Some(node, node.Next))

let iterateLinkedListReverse (lst: LinkedList<'a>) =
    lst.Last
    |> Seq.unfold (fun node -> if node = null then None else Some(node, node.Previous))


let kvp (a, b) = KeyValuePair<_, _>(a, b)

let (<!>) (p: Parser<_, _>) label : Parser<_, _> =
    fun stream ->
        printfn $"%A{stream.Position}: Entering %s{label}"
        let reply = p stream
        printfn $"%A{stream.Position}: Leaving %s{label} (%A{reply.Status})"
        reply
