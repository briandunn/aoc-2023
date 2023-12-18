module Sixteen

type Splitter =
    | Vertical
    | Horizontal

type Mirror =
    | SWNE
    | NWSE

type Tile =
    | Splitter of Splitter
    | Mirror of Mirror

type Coords = int * int

type Cardinal =
    | N
    | E
    | S
    | W

let tiles =
    Map.ofList [ '|', Splitter Vertical
                 '-', Splitter Horizontal
                 '/', Mirror SWNE
                 '\\', Mirror NWSE ]

type Grid =
    { tiles: Map<Coords, Tile>
      dims: int * int }

let parse lines =
    let lines = lines |> Seq.map Seq.toList |> Seq.toList
    let dims = lines |> Seq.head |> Seq.length, Seq.length lines

    let grid =
        seq {
            for y, line in Seq.indexed lines do
                for x, c in Seq.indexed line do
                    let tile = Map.tryFind c tiles

                    if Option.isSome tile then
                        yield (x, y), Option.get tile

        }
        |> Map.ofSeq


    { tiles = grid; dims = dims }

let print (w, h) energized =
    seq {
        for y in 0..h do
            for x in 0..w do
                if List.contains (x, y) energized then
                    "#"
                else
                    " "

            "\n"
    }
    |> String.concat ""
    |> printfn "%s"


let nextHeading direction tile =
    match tile with
    | Some (Splitter Vertical) ->
        match direction with
        | E
        | W -> [ N; S ]
        | c -> [ c ]
    | Some (Splitter Horizontal) ->
        match direction with
        | N
        | S -> [ E; W ]
        | c -> [ c ]
    | Some (Mirror SWNE) ->
        match direction with
        | N -> [ E ]
        | E -> [ N ]
        | S -> [ W ]
        | W -> [ S ]
    | Some (Mirror NWSE) ->
        match direction with
        | N -> [ W ]
        | E -> [ S ]
        | S -> [ E ]
        | W -> [ N ]
    | None -> [ direction ]

let step (x, y) =
    function
    | N -> (x, y - 1), N
    | E -> (x + 1, y), E
    | S -> (x, y + 1), S
    | W -> (x - 1, y), W

let one lines =
    let { tiles = tiles; dims = (w, h) } = parse lines

    let inBounds (x, y) = x >= 0 && x < w && y >= 0 && y < h

    let rec travel visited =
        function
        | (start, direction) as head :: rest when
            inBounds start
            && (visited |> Set.contains head |> not)
            ->
            tiles
            |> Map.tryFind start
            |> nextHeading direction
            |> List.map (step start)
            |> List.append rest
            |> travel (Set.add (start, direction) visited)

        | _ :: rest -> travel visited rest

        | [] -> visited


    [ (0, 0), E ]
    |> travel Set.empty
    |> Set.map fst
    |> Set.count
