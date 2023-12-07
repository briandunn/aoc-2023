module Six

let parse: string seq -> (int * int) seq =
    Seq.toList
    >> function
        | [ time; distance ] ->
            let time = time |> String.parseInts
            let distance = distance |> String.parseInts
            Seq.map2 Tuple.init time distance
        | _ -> Seq.empty

let one : string seq -> int =
    let distance charge duration = charge * (duration - charge)

    let wins (time, distanceToBeat) =
        seq {
            for ms in 1 .. time - 1 do
                if distance ms time > distanceToBeat then
                    yield ms
        }
        |> Seq.length

    parse >> Seq.map wins >> Seq.fold (*) 1

let two lines = 0