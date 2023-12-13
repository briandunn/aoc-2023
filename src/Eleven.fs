module Eleven

let parse: string seq -> bool Grid.Grid =
    Grid.parse
    <| function
        | '#' -> true
        | '.' -> false
        | _ -> failwith "invalid tile"

let print =
    Grid.rows
    >> Seq.iter (
        Seq.map (fun b -> if b then "#" else ".")
        >> String.concat ""
        >> printfn "%s"
    )


let expand =
  let rowsToColumns = Grid.fromRows >> Grid.columns
  let map (row: bool seq) =
      if row |> Seq.contains true then
          seq { row }
      else
          seq {
              row
              row
          }

  Grid.rows
  >> Seq.map map
  >> Seq.concat
  >> rowsToColumns
  >> Seq.map map
  >> Seq.concat
  >> Grid.fromColumns



let one lines =
    let image = parse lines


    image
    |> expand
    |> print



    0
