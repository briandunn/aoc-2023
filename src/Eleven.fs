module Eleven

let parse: string seq -> bool Grid.Grid =
    Grid.parse
    <| function
        | '#' -> true
        | '.' -> false
        | _ -> failwith "invalid tile"

let print =
    Grid.rows
    >> Seq.iter (
        Seq.map (fun b -> if b then "#" else ".")
        >> String.concat ""
        >> printfn "%s"
    )


let expand =
    let map (row: bool seq) =
        if row |> Seq.contains true then
            seq { row }
        else
            seq {
                row
                row
            }

    Grid.rows
    >> Seq.map map
    >> Seq.concat
    >> Grid.fromRows
    >> Grid.columns
    >> Seq.map map
    >> Seq.concat
    >> Grid.fromColumns


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
    >> expand
    >> Grid.indexes id
    >> Seq.toList
    >> combinations 2
    >> List.choose shortestPath
    >> List.sum
