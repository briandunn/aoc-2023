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
