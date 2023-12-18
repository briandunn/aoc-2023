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
                if Seq.contains (x, y) energized then
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

let travel start direction grid =
    let { tiles = tiles; dims = (w, h) } = grid

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
            |> travel (Set.add head visited)

        | _ :: rest -> travel visited rest

        | [] -> visited


    [ start, direction ] |> travel Set.empty

let one: string seq -> int =
    parse
    >> travel (0, 0) E
    >> Set.map fst
    >> Set.count

let two lines =
    let grid = parse lines

    let { dims = (w, h) } = grid

    let map (start, direction) =
        grid
        |> travel start direction
        |> Set.map fst
        |> Set.count

    seq {
        for x in 0..w do
            yield ((x, 0), S)

        for x in 0..w do
            yield ((x, h - 1), N)

        for y in 0..h do
            yield ((0, y), E)

        for y in 0..h do
            yield ((w - 1, y), W)
    }
    |> Seq.map map
    |> Seq.max
