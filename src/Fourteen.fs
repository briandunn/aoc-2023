module Fourteen

type Coords = int * int

type Rock =
    | Cube
    | Ball

type Dish = Map<Coords, Rock>
type Axis = Map<int, Map<int, Rock>>

let parse lines : Dish =
    seq {
        for y, line in Seq.indexed lines do
            for x, c in Seq.indexed line do
                match c with
                | '#' -> yield (x, y), Cube
                | 'O' -> yield (x, y), Ball
                | _ -> ()

    }
    |> Map.ofSeq


let toAxis groupBy map =
    let map (y, cells) =
        y,
        (cells
         |> Seq.map (fun (coords, rock) -> map coords, rock)
         |> Map.ofSeq)

    Map.toSeq
    >> Seq.groupBy (fst >> groupBy)
    >> Seq.map map
    >> Map.ofSeq

let rows: Dish -> Axis = toAxis snd fst

let columns: Dish -> Axis = toAxis fst snd

let columnsToDish (columns: Axis) : Dish =
    seq {
        for x, column in Map.toSeq columns do
            for y, rock in Map.toSeq column do
                yield (x, y), rock
    }
    |> Map.ofSeq

let print dish =
    if not (Map.isEmpty dish) then
        let h = dish |> Map.keys |> Seq.map snd |> Seq.max
        let w = dish |> Map.keys |> Seq.map fst |> Seq.max

        seq {
            for y in 0..h do
                for x in 0..w do
                    yield
                        (dish
                         |> Map.tryFind (x, y)
                         |> Option.map (function
                             | Cube -> "#"
                             | Ball -> "O")
                         |> Option.defaultValue ".")

                yield "\n"
        }
        |> String.concat ""
        |> printfn "%s"

let tilt =
    let foldRows (columns: Axis) (y: int) (rocks: Map<int, Rock>) : Axis =
        let foldColumns (columns: Axis) (x: int) (rock: Rock) : Axis =
            let changeColumn (column: Map<int, Rock>) : Map<int, Rock> =
                match rock with
                | Cube -> Map.add y Cube
                | Ball when Map.isEmpty column -> Map.add 0 Ball
                | Ball -> Map.add (column |> Map.keys |> Seq.max |> ((+) 1)) Ball
                <| column

            Map.change
                x
                (Option.defaultValue Map.empty
                 >> changeColumn
                 >> Some)
                columns

        Map.fold foldColumns columns rocks

    rows
    >> Map.fold foldRows Map.empty
    >> columnsToDish

let load (dish: Dish) =
    let dish = Map.toSeq dish
    let height = dish |> Seq.map (fst >> snd) |> Seq.max |> ((+) 1)

    dish
    |> (Seq.filter (snd >> (=) Ball)
        >> Seq.map (fst >> snd >> ((-) height))
        >> Seq.sum)

let one: string seq -> int = parse >> tilt >> load
