module Twelve

type Status =
    | Unknown
    | Operational
    | Damaged

type Record =
    { statuses: Status seq
      damagedGroupLengths: int seq }

let parse: string seq -> Record seq =
    let map =
        String.split ' '
        >> Seq.toList
        >> function
            | [ springs; damaged ] ->
                { statuses =
                    springs
                    |> Seq.map (function
                        | '?' -> Unknown
                        | '.' -> Operational
                        | '#' -> Damaged
                        | _ -> failwith "bad input")
                  damagedGroupLengths = String.parseInts damaged }
            | _ -> failwith "bad input"




    Seq.map map

let one lines =
    parse lines |> printfn "%A"

    0
