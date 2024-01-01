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
        [ for i in stop..start -> f i ]
    else
        [ for i in start..stop -> f i ] |> List.rev

let border =
    let fold path (direction, meters) =
        let next (x, y) =
            match direction with
            | Right -> range x (x + meters) (fun x -> (x, y))
            | Left -> range x (x - meters) (fun x -> (x, y))
            | Up -> range y (y - meters) (fun y -> (x, y))
            | Down -> range y (y + meters) (fun y -> (x, y))

        match path with
        | head :: rest -> (next head) @ rest
        | [] -> []

    Seq.fold fold [ 0, 0 ] >> set

let columns: (int * int) seq -> (int * int Set) seq =
    let map (col, pts) = (col, pts |> Seq.map snd |> set)

    Seq.groupBy fst >> Seq.map map

type Feature =
    | Run of int Set
    | Cross

let crossings: (int * int Set) seq -> (int * (int list)) seq =
    let fold left right (prev, crossings) =
        function
        | y when Set.contains y right && Set.contains y left -> (None, y :: crossings)
        | y when Set.contains y left && prev = Some Right -> (None, y :: crossings)
        | y when Set.contains y right && prev = Some Left -> (None, y :: crossings)
        | y when Set.contains y left -> (Some Left, crossings)
        | y when Set.contains y right -> (Some Right, crossings)
        | _ -> (prev, crossings)

    let map =
        function
        | [| (_, left); (x, ys); (_, right) |] ->
            x,
            Seq.fold (fold left right) (None, []) ys
            |> snd
            |> List.sort
        | _ -> failwith "not enough columns"

    Seq.windowed 3 >> Seq.map map

let isEven n = n % 2 = 0

let between start stop = range (start + 1) (stop - 1) id

let fill border =

    // there isn't enough information in a single column.
    // if a wall segment column leaves the way it came it did not cross the path counts as 0 or 2
    // if a wall segment crosses the column it counts as 1

    // turn a column into runs and crosses - need a sliding window of three columns


    let columns = columns border

    let crossings = crossings columns |> Ten.p "crossings"

    let rec loop =
        function
        | head :: neck :: rest when rest |> List.length |> isEven -> (between head neck) @ loop (neck :: rest)
        | _ :: rest -> loop rest
        | [] -> []


    let map (x, ys) =
        let map y = (x, y)
        ys |> loop |> List.map map |> set

    crossings
    |> Seq.map map
    |> Set.unionMany
    |> Set.union border
    |> Set.toList

let one lines =
    parse lines
    |> Seq.toList
    |> border
    |> fill
    |> print
    |> printfn "%s"

    0
