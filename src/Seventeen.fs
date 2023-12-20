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

let parse: string seq -> int Grid.Grid = Grid.parse (string >> int)

let destination grid =
    Array2D.length1 grid - 1, Array2D.length2 grid - 1

let connected move =
    Seq.choose move [ Left; Straight; Right ]

let inBounds (maxX, maxY) (x, y) =
    x >= 0 && x <= maxX && y >= 0 && y <= maxY

let minimumHeatLoss move grid =
    let destination = destination grid
    let inBounds = inBounds destination

    let connected =
        let filter crucible = inBounds crucible.position
        move >> connected >> Seq.filter filter

    let weight { position = (x, y) } = Array2D.get grid x y

    let updateWeights currentWeight unvisited =
        let fold unvisited connected =
            let nextWeight = currentWeight + weight connected

            let change =
                Option.map (min nextWeight)
                >> Option.defaultValue nextWeight
                >> Some

            Map.change connected change unvisited

        Seq.fold fold unvisited

    let east =
        { position = (1, 0)
          heading = E
          stepCount = 1 }

    let south =
        { position = (0, 1)
          heading = S
          stepCount = 1 }

    let rec loop visited weights =
        let isUnvisited crucible = not <| Set.contains crucible visited

        weights
        |> Map.toSeq
        |> Seq.minBy snd
        |> function
            | { position = position }, weight when position = destination -> weight
            | current, weight ->
                let weights =
                    current
                    |> connected
                    |> Seq.filter isUnvisited
                    |> updateWeights weight weights
                    |> Map.remove current

                loop (Set.add current visited) weights

    [ east, weight east
      south, weight south ]
    |> Map.ofList
    |> loop Set.empty

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
        | Right when stepCount <= 4 -> None
        | Straight -> (stepCount + 1) |> next |> Some
        | _ -> 1 |> next |> Some

    parse >> minimumHeatLoss move
