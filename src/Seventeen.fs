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
    let max f = keys |> Seq.map f |> Seq.max

    let init y x = Map.find (x, y) tiles

    Array2D.init (max fst + 1) (max snd + 1) init


let destination grid =
    Array2D.length1 grid - 1, Array2D.length2 grid - 1

let connected move =
    Seq.choose move [ Left; Straight; Right ]

let inBounds (maxX, maxY) (x, y) =
    x >= 0 && x <= maxX && y >= 0 && y <= maxY

let printPath pathMap last grid =
    let rec loop head path =
        match Map.tryFind head pathMap with
        | Some el -> loop el (el :: path)
        | None -> path

    let fold acc crucible =
        Map.add crucible.position crucible.heading acc

    let path = [] |> loop last |> List.fold fold Map.empty

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

let minimumHeatLoss move grid =
    let destination = destination grid
    let inBounds = inBounds destination

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

        weights
        |> Map.toSeq
        |> Seq.minBy snd
        |> function
            | { position = position } as crucible, weight when position = destination ->
                printPath path crucible grid

                weight
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

let step (x, y) =
    function
    | N -> (x, y - 1)
    | E -> (x + 1, y)
    | S -> (x, y + 1)
    | W -> (x - 1, y)

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

let nextCrucible stepCount direction crucible =

    let nextHeading = heading crucible.heading direction

    { stepCount = stepCount
      heading = nextHeading
      position = step crucible.position nextHeading }

let one: string seq -> int =

    let move ({ stepCount = stepCount } as crucible) direction =
        let next stepCount =
            nextCrucible stepCount direction crucible

        match direction with
        | Straight when stepCount >= 3 -> None
        | Straight -> (stepCount + 1) |> next |> Some
        | _ -> 1 |> next |> Some

    parse >> minimumHeatLoss move

let two: string seq -> int =

    let move ({ stepCount = stepCount } as crucible) direction =
        let next stepCount =
            nextCrucible stepCount direction crucible

        match direction with
        | Straight when stepCount >= 10 -> None
        | Left
        | Right when stepCount < 4 -> None
        | Straight -> (stepCount + 1) |> next |> Some
        | _ -> 1 |> next |> Some

    parse >> minimumHeatLoss move
