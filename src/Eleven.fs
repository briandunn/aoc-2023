module Eleven

type Coords = int * int

let parse (lines: string seq) : Coords Set =
    Set.ofSeq
    <| seq {
        for y, line in Seq.indexed lines do
            for x, c in Seq.indexed line do
                if c = '#' then yield x, y
    }

let between min max x = x > min && x < max

let expand n =
    let expand get update image =
        let occupied = Seq.map get image
        let max = Seq.max occupied

        let blanks = Set.difference (Set.ofSeq <| seq { 0..max }) (Set.ofSeq occupied)

        let mapi i (blankA, blankB) =
            let shift = (n - 1L) * (int64 i)

            image
            |> Seq.filter (get >> (between blankA blankB))
            |> Seq.map (update ((+) shift))

        seq {
            -1
            yield! blanks
            max + 1
        }
        |> Seq.pairwise
        |> Seq.mapi mapi
        |> Seq.concat
        |> Set.ofSeq

    expand Tuple.first (fun f (x, y) -> f x, y)
    >> expand Tuple.second (fun f (x, y) -> x, f y)


let rec combinations n list =
    match list with
    | [] -> []
    | list when n = 1 -> [ for item in list -> [ item ] ]
    | head :: rest ->
        (rest
         |> combinations (n - 1)
         |> List.map (fun l -> head :: l))
        @ combinations n rest

let shortestPath =
    function
    | [ (xa, ya); (xb, yb) ] -> abs (xb - xa) + abs (yb - ya) |> Some
    | _ -> None

let one: string seq -> int =
    parse
    >> expand 2L
    >> Seq.toList
    >> combinations 2
    >> List.choose shortestPath
    >> List.sum // 10494813
    >> (int)


let two (lines: string seq) : int =
    lines
    |> parse
    |> expand 1000000L
    |> Seq.toList
    |> combinations 2
    |> List.choose shortestPath
    |> List.sum // 10494813
    |> printfn "%A"

    0
