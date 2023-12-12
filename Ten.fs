module Ten

type Coords = int * int

type Cardinal =
    | North
    | East
    | South
    | West

type Tile =
    | Pipe of Cardinal Set
    | Animal
    | Ground


let inline p label x =
    printfn "%s: %A" label x
    x

let parse lines : Tile array2d =
    let charToTile =
        function
        | '|' -> Pipe(Set.ofList [ North; South ])
        | 'L' -> Pipe(Set.ofList [ North; East ])
        | 'J' -> Pipe(Set.ofList [ North; West ])
        | '-' -> Pipe(Set.ofList [ East; West ])
        | '7' -> Pipe(Set.ofList [ West; South ])
        | 'F' -> Pipe(Set.ofList [ East; South ])
        | 'S' -> Animal
        | '.' -> Ground
        | _ -> failwith "invalid tile"

    let tiles =
        Map.ofSeq
        <| seq {
            for y, line in Seq.indexed lines do
                for x, c in Seq.indexed line -> (x, y), charToTile c
        }


    let keys = Map.keys tiles
    let max f = keys |> Seq.map f |> Seq.max

    let init x y = Map.find (x, y) tiles

    Array2D.init (max Tuple.first + 1) (max Tuple.second + 1) init


let tryFindIndex predicate array =
    seq {
        for x in 0 .. Array2D.length1 array - 1 do
            for y in 0 .. Array2D.length2 array - 1 do
                if predicate <| Array2D.get array x y then
                    (x, y)
    }
    |> Seq.tryHead

let inRange (x, y) grid =
    x < Array2D.length1 grid
    && x >= 0
    && y < Array2D.length2 grid
    && y > 0

let item (x, y) grid = Array2D.get grid x y

let openSides =
    function
    | Pipe set -> set
    | Animal -> Set.ofList [ North; South; East; West ]
    | Ground -> Set.empty

let connectsFrom ordinal tile =
    match ordinal, tile with
    | North, Pipe set when Set.contains South set -> true
    | South, Pipe set when Set.contains North set -> true
    | East, Pipe set when Set.contains West set -> true
    | West, Pipe set when Set.contains East set -> true
    | _, Animal -> true
    | _ -> false

let furthest: Coords seq seq -> int =
    Seq.map Seq.length
    >> Seq.countBy id
    >> Seq.filter (fun (_, count) -> count = 2)
    >> Seq.map (fun (length, _) -> length / 2)
    >> Seq.max
    >> ((+) 1)

let opposite =
    function
    | North -> South
    | South -> North
    | East -> West
    | West -> East

let one (lines: string seq) : int =
    let grid = parse lines

    let inBounds (x, y) =
        if x >= 0
           && y >= 0
           && x < Array2D.length1 grid
           && y < Array2D.length2 grid then
            Some(x, y)
        else
            None

    let coords ((x, y): Coords) : Cardinal -> Coords option =
        function
        | North -> (x, y - 1)
        | East -> (x + 1, y)
        | South -> (x, y + 1)
        | West -> (x - 1, y)
        >> inBounds

    let path (from: Cardinal) (start: Coords) =
        let unfold (from, start) =
            let bind cardinal =
                let map next = (from, start), (cardinal, next)

                cardinal |> coords start |> Option.map map

            grid
            |> item start
            |> openSides
            |> Set.filter ((<>) (opposite from))
            |> Seq.tryExactlyOne
            |> Option.bind bind

        Seq.unfold unfold (from, start)

    let notAnimal (_, coords) = item coords grid <> Animal

    let fromAnimal animal =
        let unfold cardinals : ((Cardinal * Coords) seq * Cardinal Set) option =
            let map cardinal coords =
                coords |> path cardinal |> Seq.takeWhile notAnimal

            cardinals
            |> Seq.tryHead
            |> Option.map (fun cardinal ->
                let path =
                    cardinal
                    |> coords animal
                    |> Option.map (map cardinal)
                    |> Option.defaultValue Seq.empty

                let cardinals = Seq.tail cardinals

                path
                |> Seq.tryLast
                |> function
                    | Some (cardinal, _) ->
                        path,
                        cardinals
                        |> Seq.filter ((<>) (opposite cardinal))
                        |> Set.ofSeq
                    | None -> path, Set.ofSeq cardinals)

        grid
        |> item animal
        |> openSides
        |> Seq.unfold unfold

    grid
    |> tryFindIndex ((=) Animal)
    |> Option.map (fromAnimal >> Seq.map Seq.length >> Seq.max >> ((+) 1) >> (fun max -> max / 2))
    |> Option.defaultValue 0

let two (lines: string seq) : int = 0
