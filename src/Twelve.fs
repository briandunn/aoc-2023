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

let damagedGroupLengths: Status seq -> int list =
    let fold (current, lengths) status =
        match status with
        | Damaged ->
            match current with
            | None -> (Some 1, lengths)
            | Some n -> (Some(n + 1), lengths)

        | _ ->
            match current with
            | None -> (None, lengths)
            | Some n -> (None, n :: lengths)

    Seq.fold fold (None, [])
    >> function
        | (None, lengths) -> lengths
        | (Some n, lengths) -> n :: lengths
    >> List.rev

let satisfies
    { statuses = statuses
      damagedGroupLengths = lengths }
    =
    statuses
    |> damagedGroupLengths
    |> ((=) (Seq.toList lengths))

let permute ({ statuses = statuses } as r) =
    let slots =
        statuses
        |> Seq.filter ((=) Unknown)
        |> Seq.length

    let rec permute length options =
        if length = 0 then
            []

        else if length = 1 then
            List.map List.singleton options
        else
            options
            |> (permute (length - 1))
            |> List.map (fun permutation -> List.map (fun option -> option :: permutation) options)
            |> List.concat

    let substitute statuses permutation =
        let fold (out, permutation) status =
            match status with
            | Unknown -> Seq.head permutation :: out, Seq.tail permutation
            | status -> status :: out, permutation

        Seq.fold fold ([], permutation) statuses |> Tuple.first |> List.rev

    permute slots [ Damaged; Operational ]
    |> List.map (fun permutation -> { r with statuses = substitute statuses permutation })

let one lines =
    parse lines
    |> Seq.map (permute >> Seq.filter satisfies >> Seq.length)
    |> Seq.sum
