module Grid

type 'a Grid = 'a array2d

let parse (parse: char -> 'a) (lines: string seq) : 'a Grid =

    let tiles =
        Map.ofSeq
        <| seq {
            for y, line in Seq.indexed lines do
                for x, c in Seq.indexed line -> (x, y), parse c
        }


    let keys = Map.keys tiles
    let max f = keys |> Seq.map f |> Seq.max

    let init x y = Map.find (x, y) tiles

    Array2D.init (max Tuple.first + 1) (max Tuple.second + 1) init


let indexes (predicate: 'a -> bool) (array: 'a Grid) =
    seq {
        for x in 0 .. Array2D.length1 array - 1 do
            for y in 0 .. Array2D.length2 array - 1 do
                if predicate <| Array2D.get array x y then
                    (x, y)
    }


let rows (grid: 'a Grid) =
    seq {
        for y in 0 .. Array2D.length2 grid - 1 do
            yield
                seq {
                    for x in 0 .. Array2D.length1 grid - 1 do
                        yield Array2D.get grid x y
                }
    }

let fromRows (rows: 'a seq seq) : 'a Grid =
  let h = Seq.length rows
  let w = rows |> Seq.head |> Seq.length

  let init x y = rows |> Seq.item y |> Seq.item x

  Array2D.init w h init

let fromColumns (columns: 'a seq seq) : 'a Grid =
  let w = Seq.length columns
  let h = columns |> Seq.head |> Seq.length

  let init x y = columns |> Seq.item x |> Seq.item y

  Array2D.init w h init


let columns (grid: 'a Grid) =
    seq {
        for x in 0 .. Array2D.length1 grid - 1 do
            yield
                seq {
                    for y in 0 .. Array2D.length2 grid - 1 do
                        yield Array2D.get grid x y
                }
    }