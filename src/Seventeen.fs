module Seventeen

type Direction =
    | Left
    | Right
    | Straight

type Cardinal =
    | N
    | S
    | E
    | W

type Coords = int * int

type Crucible =
    { position: Coords
      heading: Cardinal
      stepCount: int }

let parse lines =

    let tiles =
        Map.ofSeq
        <| seq {
            for y, line in Seq.indexed lines do
                for x, c in Seq.indexed line -> (x, y), c |> string |> int
        }

    let keys = Map.keys tiles
    let length f = keys |> Seq.map f |> Seq.max |> (+) 1

    let init y x = Map.find (x, y) tiles

    Array2D.init (length snd) (length fst) init


let destination grid =
    Array2D.length1 grid - 1, Array2D.length2 grid - 1

let connected move =
    Seq.choose move [ Left; Straight; Right ]

let inBounds grid (x, y) =
    let maxX = Array2D.length1 grid
    let maxY = Array2D.length2 grid
    x >= 0 && x < maxX && y >= 0 && y < maxY

let printPath pathMap last grid =
    let rec loop head path =
        match Map.tryFind head pathMap with
        | Some el -> loop el (el :: path)
        | None -> path

    let fold acc crucible =
        Map.add crucible.position crucible.heading acc

    let path = [ last ] |> loop last |> List.fold fold Map.empty

    seq {
        for x in 0 .. (fst last.position) do
            for y in 0 .. (snd last.position) do
                match Map.tryFind (x, y) path with
                | None -> Array2D.get grid x y |> string
                | Some heading ->
                    match heading with
                    | N -> "^"
                    | S -> "v"
                    | E -> ">"
                    | W -> "<"

            "\n"
    }
    |> String.concat ""
    |> printfn "%s"

let minimumHeatLoss move isDone grid =
    let inBounds = inBounds grid

    let connected =
        let filter crucible = inBounds crucible.position
        move >> connected >> Seq.filter filter

    let weight { position = (x, y) } = Array2D.get grid x y

    let east =
        { position = (1, 0)
          heading = E
          stepCount = 1 }

    let south =
        { position = (0, 1)
          heading = S
          stepCount = 1 }

    let rec loop visited path weights =
        let isUnvisited crucible = not <| Set.contains crucible visited

        if Map.isEmpty weights then
            None
        else
            weights
            |> Map.toSeq
            |> Seq.minBy snd
            |> function
                // | { position = position; stepCount = stepCount } as crucible, weight when position = destination && stepCount >= 4 ->
                | crucible, weight when isDone crucible ->
                    printPath path crucible grid

                    Some weight
                | current, currentWeight ->
                    let updateWeights (path, unvisited) connected =
                        let nextWeight = currentWeight + weight connected

                        Map.tryFind connected unvisited
                        |> function
                            | Some previousWeight when previousWeight <= nextWeight -> path, unvisited
                            | _ -> Map.add connected current path, Map.add connected nextWeight unvisited

                    let path, weights =
                        current
                        |> connected
                        |> Seq.filter isUnvisited
                        |> Seq.fold updateWeights (path, weights)

                    loop (Set.add current visited) path (Map.remove current weights)

    [ east, weight east
      south, weight south ]
    |> Map.ofList
    |> loop Set.empty Map.empty

let step n (x, y) =
    function
    | N -> (x, y - n)
    | E -> (x + n, y)
    | S -> (x, y + n)
    | W -> (x - n, y)

let heading heading direction =
    match heading, direction with
    | E, Left -> N
    | E, Right -> S
    | S, Left -> E
    | S, Right -> W
    | W, Left -> S
    | W, Right -> N
    | N, Left -> W
    | N, Right -> E
    | heading, Straight -> heading

let nextCrucible steps direction crucible =

    let nextHeading = heading crucible.heading direction

    let stepCount =
        match direction with
        | Straight -> crucible.stepCount + steps
        | _ -> steps


    { stepCount = stepCount
      heading = nextHeading
      position = step steps crucible.position nextHeading }

let one lines =

    let move ({ stepCount = stepCount } as crucible) direction =
        let next stepCount =
            nextCrucible stepCount direction crucible

        match direction with
        | Straight when stepCount >= 3 -> None
        | _ -> 1 |> next |> Some

    let grid = parse lines
    let destination = destination grid

    grid |> minimumHeatLoss move (fun {position = position} -> position = destination ) |> Option.defaultValue -1


// only allow nav 4 steps
// have to count the cost of the nav as the sum of 4 tiles.
// would be easier if cost was stored in crucible
let two lines =

    let move ({ stepCount = stepCount } as crucible) direction =
        let next stepCount =
            nextCrucible stepCount direction crucible

        match direction with
        | Straight when stepCount >= 10 -> None
        | Left
        | Right when stepCount < 4 -> None
        | Straight when stepCount >= 4 -> 1 |> next |> Some
        // | Straight -> 4 |> next |> Some
        | _ -> 1 |> next |> Some

    let grid = parse lines
    let destination = destination grid

    grid |> minimumHeatLoss move (fun {position = position; stepCount = stepCount} -> position = destination && stepCount >= 4 ) |> Option.defaultValue -1