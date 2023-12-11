module Nine

let parse : string seq -> int list seq = Seq.map (Regex.matches "\-?\d+" >> Seq.map int >> Seq.toList)

let rec differences seq =
    if Seq.forall ((=) 0) seq then
        []
    else
        seq
        :: (seq
            |> List.pairwise
            |> List.map (fun (a, b) -> b - a)
            |> differences)

let rec extrapolate difference =
    function
    | row :: rest ->
        let rev = List.rev row
        let tail = List.head rev
        let difference = tail + difference

        (List.rev (difference :: rev))
        :: (extrapolate difference rest)
    | [] -> []

let predict =
    differences
    >> List.rev
    >> extrapolate 0
    >> List.tryLast
    >> Option.bind List.tryLast

let one: string seq -> int = parse >> Seq.choose predict >> Seq.sum
let two: string seq -> int = parse >> Seq.choose (List.rev >> predict) >> Seq.sum
