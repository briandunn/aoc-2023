module String

open System
open System.Text.RegularExpressions

let numRe = new Regex("-?\d+")

let parseNumbers: string -> int64 seq =
    let map (v: string) =
        try
            Convert.ToInt64(v)
        with
        | :? OverflowException ->
            printfn "overflow: %s" v
            0

    Regex.matches "\d+" >> Seq.map map

let parseInts: string -> int seq = Regex.matches "\d+" >> Seq.map int

let split (c: char) (s: string) : string seq = s.Split(c)