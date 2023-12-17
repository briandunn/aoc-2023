module Fifteen

let hash =
    let fold current ascii = (current + ascii) * 17 % 256

    Seq.map int >> Seq.fold fold 0


let one: string seq -> int =
    Seq.head
    >> String.split ','
    >> Seq.map hash
    >> Seq.sum


let focusingPower series =
    let mapi i =
        function
        | [] -> 0
        | box -> box |> List.mapi (fun j (l, fl) -> (1 + i) * (1 + j) * fl) |> List.sum

    series |> Array.mapi mapi |> Array.sum

type Operation =
    | Add of int
    | Remove

type Instruction = { label: string; operation: Operation }

let parse instruction =
    instruction
    |> Regex.groups "^([a-z]+)(=|\-)(\d+)?"
    |> Seq.tail
    |> Seq.toList
    |> function
        | [ label; "="; value ] ->
            { label = label
              operation = Add(int value) }
        | [ label; "-"; "" ] -> { label = label; operation = Remove }
        | _ -> failwith "Invalid instruction"

let two: string seq -> int =

    let fold (series: (string * int) list array) { label = label; operation = operation } =
        let h = hash label
        let box = Array.get series h

        box
        |> List.tryFindIndex (fun (l, _) -> l = label)
        |> (match operation with
            | Add value ->
                function
                | Some index -> List.updateAt index (label, value) box
                | None -> box @ [ label, value ]
            | Remove ->
                function
                | Some index -> List.removeAt index box
                | None -> box)
        |> Array.set series h

        series

    (Seq.head
     >> String.split ','
     >> Seq.map parse
     >> Seq.fold fold (Array.init 256 (fun _ -> []))
     >> focusingPower)
