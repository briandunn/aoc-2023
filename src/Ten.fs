module Ten

type Coords = int * int

type Cardinal =
    | North
    | East
    | South
    | West

type Pipe = Cardinal Set

type Tile =
    | Pipe of Pipe
    | Animal
    | Ground

type Grid = Tile array2d

let inline p label x =
    printfn "%s: %A" label x
    x

let parse lines : Grid =
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


let indexes predicate array =
    seq {
        for x in 0 .. Array2D.length1 array - 1 do
            for y in 0 .. Array2D.length2 array - 1 do
                if predicate <| Array2D.get array x y then
                    (x, y)
    }

let tryFindIndex predicate = indexes predicate >> Seq.tryExactlyOne



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

let opposite =
    function
    | North -> South
    | South -> North
    | East -> West
    | West -> East

let coords ((x, y): Coords) : Cardinal -> Coords =
    function
    | North -> (x, y - 1)
    | East -> (x + 1, y)
    | South -> (x, y + 1)
    | West -> (x - 1, y)

let loopsFromAnimal grid =
    let inBounds (x, y) =
        if x >= 0
           && y >= 0
           && x < Array2D.length1 grid
           && y < Array2D.length2 grid then
            Some(x, y)
        else
            None

    let follow (from: Cardinal) (start: Coords) =
        let unfold (from, start) =
            let bind cardinal =
                let map next = (from, start), (cardinal, next)

                cardinal
                |> coords start
                |> inBounds
                |> Option.map map

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
            let map cardinal =
                let path =
                    let map cardinal coords =
                        coords
                        |> follow cardinal
                        |> Seq.takeWhile notAnimal

                    cardinal
                    |> coords animal
                    |> inBounds
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
                    | None -> path, Set.ofSeq cardinals

            cardinals |> Seq.tryHead |> Option.map map

        grid
        |> item animal
        |> openSides
        |> Seq.unfold unfold

    grid
    |> tryFindIndex ((=) Animal)
    |> Option.map (fromAnimal >> Seq.filter (Seq.isEmpty >> not))
    |> Option.defaultValue Seq.empty

let adjacent ((xa, ya): Coords) ((xb, yb): Coords) : bool =
    abs (xa - xb) <= 1 && abs (ya - yb) <= 1

let rec expand (grounds: Coords list) : Coords list =
    match grounds with
    | head :: rest ->
        let adjacent, rest = List.partition (adjacent head) rest

        head
        :: (adjacent
            |> List.map (fun a -> expand (a :: rest))
            |> List.concat)

    | [] -> []


let perpendicular (cardinal: Cardinal) (pipe: Pipe) : bool =
    [ cardinal; opposite cardinal ]
    |> Set.ofList
    |> Set.difference pipe
    |> Set.count
    |> ((=) 2)

let blocked ground cardinal loop grid =
    let next = (coords ground) cardinal

    Set.contains next loop
    && grid
       |> item next
       |> openSides
       |> perpendicular cardinal


let onEdge ((x, y): Coords) grid : bool =
    x = 0
    || y = 0
    || x = Array2D.length1 grid - 1
    || y = Array2D.length2 grid - 1

let reachableFrom ground =
    [ North; East; South; West ]
    |> Set.ofList
    // |> Set.filter (not << (blocked ground))
    |> Set.map (coords ground)

let isInside grid loop ground =
    false

let one: string seq -> int =
    parse
    >> loopsFromAnimal
    >> Seq.map Seq.length
    >> Seq.max
    >> ((+) 1)
    >> (fun max -> max / 2)

let two (lines: string seq) : int =
    let grid = parse lines

    let loop =
        grid
        |> loopsFromAnimal
        |> Seq.map (Seq.map Tuple.second)
        |> Seq.concat
        |> Set.ofSeq


    // pick a ground tile
    // walk east until you hit a wall or the loop
    // turn south and walk until you hit a wall or the loop
    // turn west and walk until you hit a wall or the loop


    grid
    |> indexes ((=) Ground)
    |> Seq.filter (isInside grid loop)
    |> Set.ofSeq
    // |> Set.difference loop
    |> printfn "partitioned: %A"

    0
