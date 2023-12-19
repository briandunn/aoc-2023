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
    (direction)
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

let inBounds destination (x, y) =
    x >= 0
    && x <= fst destination
    && y >= 0
    && y <= snd destination

let walk grid =
    let destination = destination grid

    let unfold starts =
        let rec loop =
            function
            | ({ position = position }, visited) :: rest when position = destination ->
                Some(Set.add position visited, rest)
            | ({ position = pos } as crucible, visited) :: rest when
                inBounds destination pos
                && not (Set.contains pos visited)
                ->
                let visited = Set.add pos visited

                let moves =
                    connected crucible
                    |> List.map (fun crucible -> (crucible, visited))

                (moves @ rest)
                |> (fun x ->
                    List.length x |> printfn "%A"
                    x)
                |> loop
            | _ :: rest -> loop rest
            | [] -> None

        loop starts

    [ E; S ]
    |> List.map (fun heading ->
        { position = (0, 0)
          heading = heading
          stepCount = 1 },
        Set.empty)
    |> Seq.unfold unfold


let one lines =
    let grid = parse lines
    let destination = destination grid
    let inBounds = inBounds destination
    let connected = connected >> List.filter (fun crucible -> inBounds crucible.position)

    let weight { position = (x, y) } = Array2D.get grid x y

    let current =
        { position = (0, 0)
          heading = E
          stepCount = 1 }

    let rec loop current visited unvisited =
        visited
        |> Seq.map (fun c -> c.position)
        |> Set.ofSeq
        |> print grid

        let connectedUnvisited node =
            let filter crucible = not (connected crucible = []) && not (Set.contains crucible visited)

            node |> connected |> List.filter filter

        current
        |> Ten.p "current"
        |> connectedUnvisited
        |> function
            | [] ->
                printfn "unvisited: %A" unvisited
                None
            | connectedUnvisited ->

                let fold unvisited connected =
                    let change =
                        function
                        | None -> Some(weight current + weight connected)
                        | Some currentWeight ->
                            currentWeight
                            |> min (weight current + weight connected)
                            |> Some

                    Map.change connected change unvisited

                let unvisited = List.fold fold unvisited connectedUnvisited

                connectedUnvisited
                |> List.minBy (fun crucible -> Map.find crucible unvisited)
                |> function
                    | { position = position } when position = destination -> Some visited
                    | next -> loop next (Set.add current visited) unvisited


    loop current Set.empty Map.empty |> printfn "%A"



    0
