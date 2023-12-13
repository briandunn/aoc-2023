module Six

let distance charge duration = charge * (duration - charge)

let wins (time, distanceToBeat) =
    seq {
        for ms in 1L .. time - 1L do
            if distance ms time > distanceToBeat then
                yield ms
    }
    |> Seq.length

let one: string seq -> int =
    let parse =
        Seq.toList
        >> function
            | [ time; distance ] ->
                let time = time |> String.parseNumbers
                let distance = distance |> String.parseNumbers
                Seq.map2 Tuple.init time distance
            | _ -> Seq.empty

    parse >> Seq.map wins >> Seq.fold (*) 1

let two: (string seq) -> int =
    let parse =
        Seq.map (Regex.matches "\d+" >> String.concat "" >> int64)
        >> Seq.toList
        >> function
            | [ time; distance ] -> [ (time, distance) ]
            | _ -> []

    parse >> Seq.map wins >> Seq.head
