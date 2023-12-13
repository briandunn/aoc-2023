module test

open NUnit.Framework
open FsUnit
open Ten

let loop =
    loopsFromAnimal
    >> Seq.map (Seq.map Tuple.second)
    >> Seq.concat
    >> Set.ofSeq

let toGrid (s: string) : Grid =
    s.Split('\n')
    |> Seq.map (fun s -> s.Trim())
    |> parse

[<SetUp>]
let Setup () = ()

[<Test>]
let isInside () =
    let grid =
        toGrid
            "F-7F7
             |.|||
             |FJ||
             ||.||
             |L-J|
             L---J"

    let loop = loop grid

    let isInside = isInside grid loop

    isInside (1, 1) |> should equal true
    isInside (2, 3) |> should equal false
