module Seventeen

type Steps =
    | One
    | Two
    | Three

type Direction =
    | Left
    | Right
    | Straight

type Cardinal =
    | N
    | S
    | E
    | W

type Move = Direction * Steps

type Coords = int * int

type Crucible =
    { position: Coords
      heading: Cardinal
      stepCount: int }

let parse: string seq -> int Grid.Grid = Grid.parse (string >> int)

let destination grid =
    Array2D.length1 grid - 1, Array2D.length2 grid - 1

let blocks =
    function
    | One -> 1
    | Two -> 2
    | Three -> 3

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

let move
    { position = current
      heading = currentHeading
      stepCount = stepCount }
    direction
    : Crucible option =
    let stepCount =
        match direction with
        | Straight -> stepCount + 1
        | _ -> 1

    let nextHeading = heading currentHeading direction

    if stepCount > 3 then
        None
    else
        Some(
            { position = step current nextHeading
              heading = nextHeading
              stepCount = stepCount }
        )

let connected from =
    Seq.choose (move from) [ Left; Straight; Right ]


let print grid path =
    seq {
        for y in 0 .. Array2D.length2 grid - 1 do
            for x in 0 .. Array2D.length1 grid - 1 do
                if Set.contains (x, y) path then
                    "*"
                else
                    Array2D.get grid x y |> string

            "\n"
    }
    |> String.concat ""
    |> printfn "%s"

let inBounds (maxX, maxY) (x, y) =
    x >= 0 && x <= maxX && y >= 0 && y <= maxY

let one lines =
    let grid = parse lines
    let destination = destination grid
    let inBounds = inBounds destination

    let connected =
        let filter crucible = inBounds crucible.position
        connected >> Seq.filter filter

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
                current
                |> connected
                |> Seq.filter isUnvisited
                |> updateWeights weight weights
                |> Map.remove current
                |> loop (Set.add current visited)

    [ east, weight east
      south, weight south ]
    |> Map.ofList
    |> loop Set.empty
