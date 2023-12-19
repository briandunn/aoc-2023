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
    List.choose (move from) [ Left; Straight; Right ]


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

// too much state in the crucible.
// Need to work with just the position.
// rather than keeping step count and heading, treat all the places you could get in 1 move as connected.
// their weight is the sum of the weights of the places you've been so far.
// so the trick is how to get the weight of each potential next place.
let one lines =
    let grid = parse lines
    let destination = destination grid
    let inBounds = inBounds destination

    let connected =
        connected
        >> List.filter (fun crucible -> inBounds crucible.position)

    let weight { position = (x, y) } = Array2D.get grid x y

    let updateWeights
        (current: Crucible)
        (unvisited: Map<Crucible, int>)
        (connections: Crucible list)
        : Map<Crucible, int> =
        let fold unvisited connected =
            let change =
                function
                | None -> Some(weight current + weight connected)
                | Some currentWeight ->
                    currentWeight
                    |> min (weight current + weight connected)
                    |> Some

            Map.change connected change unvisited

        List.fold fold unvisited connections

    let east =
        { position = (1, 0)
          heading = E
          stepCount = 1 }

    let south =
        { position = (0, 1)
          heading = S
          stepCount = 1 }

    let unfold (visited, weights) =
        let connectedUnvisited node =
            let filter crucible = not (Set.contains crucible visited)

            node |> connected |> List.filter filter

        weights
        |> Map.toList
        |> function
            | [] -> None
            | unvisited ->
                unvisited
                |> List.minBy snd
                |> fst
                |> function
                    | { position = position } as current when position = destination ->
                        let unvisited =
                            current
                            |> connectedUnvisited
                            |> updateWeights current weights

                        Some(unvisited, (Set.empty, Map.empty))
                    | current ->
                        let unvisited =
                            current
                            |> connectedUnvisited
                            |> updateWeights current weights

                        Some(unvisited, (Set.add current visited, Map.remove current unvisited))

    Seq.unfold
        unfold
        (Set.empty,
         Map.ofList [ east, weight east
                      south, weight south ])
    |> Seq.last
    |> Map.values |> Seq.toList |> printfn "%A"
    // |> Map.toSeq
    // |> Seq.tryFind (fun (crucible, weight) -> crucible.position = destination)
    // |> printfn "%A"



    0
