module Eighteen

type Direction =
    | Right
    | Left
    | Up
    | Down

let parse lines =
    let parse =
        String.split ' '
        >> Seq.toList
        >> function
            | [ direction; meters; _color ] ->
                let direction =
                    match direction with
                    | "R" -> Right
                    | "L" -> Left
                    | "U" -> Up
                    | "D" -> Down
                    | _ -> failwith "unknown direction"

                let meters = int meters
                (direction, meters)

            | _ -> failwith "unknown direction"

    seq {
        for line in lines do
            parse line
    }

let print path =
    let minX = path |> Seq.map fst |> Seq.min
    let maxX = path |> Seq.map fst |> Seq.max
    let minY = path |> Seq.map snd |> Seq.min
    let maxY = path |> Seq.map snd |> Seq.max

    printfn "%A" path

    seq {
        for y in minY..maxY do
            for x in minX..maxX do
                if List.contains (x, y) path then
                    "#"
                else
                    "."

            "\n"
    }
    |> String.concat ""

let range start stop f =
    if start > stop then
        [for i in stop..start -> f i]
    else
        [for i in start..stop -> f i] |> List.rev
let move =
    let fold path (direction, meters) =
        let next (x,y) =
            match direction with
            | Right -> range x (x + meters) (fun x -> (x, y))
            | Left -> range x (x - meters) (fun x -> (x, y))
            | Up -> range y (y + meters) (fun y -> (x, y))
            | Down -> range y (y - meters) (fun y -> (x, y))

        match path with
        | head :: rest -> (next head) @ rest
        | [] -> []

    Seq.fold fold [0,0]

let one lines =
    parse lines |> Seq.toList |> move |> print |> printfn "%s"
    0
