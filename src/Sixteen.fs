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


let one lines =
    let { tiles = tiles; dims = (w, h) } = parse lines |> Ten.p "parse"

    let rec travel ((x, y) as start) direction =
        let head direction =
            match direction with
            | N -> travel (x, y - 1) N
            | E -> travel (x + 1, y) E
            | S -> travel (x, y + 1) S
            | W -> travel (x - 1, y) W

        if x < 0 || x >= w || y < 0 || y >= h then
            []
        else
            (start, direction)
            :: (match (Map.tryFind start tiles) with
                | Some (Splitter Vertical) ->
                    match direction with
                    | E
                    | W -> (head N) @ (head S)
                    | c -> head c
                | Some (Splitter Horizontal) ->
                    match direction with
                    | N
                    | S -> (head W) @ (head E)
                    | c -> head c
                | Some (Mirror SWNE) ->
                    match direction with
                    | N -> head E
                    | E -> head N
                    | S -> head W
                    | W -> head S
                | Some (Mirror NWSE) ->
                    match direction with
                    | N -> head W
                    | E -> head S
                    | S -> head E
                    | W -> head N
                | None -> head direction)


    // travel (0, 0) E |> print (w, h)
    travel (0, 0) E |> printfn "%A" 

    1
