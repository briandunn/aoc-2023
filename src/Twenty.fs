module Twenty

type Broadcaster = string list

type FlipFlop =
    { name: string
      state: bool
      outputs: string list }

type Conjunction =
    { inputStates: Map<string, bool>
      name: string
      outputs: string list }

type Module =
    | Broadcaster of Broadcaster
    | FlipFlop of FlipFlop
    | Conjunction of Conjunction

let parse: string seq -> Module seq =
    let buildModule moduleType name destinations =
      let destinations = destinations |> Regex.matches "([a-z]+)" |> Seq.toList

      match moduleType with
      | "" -> Broadcaster destinations |> Some
      | "%" -> FlipFlop { name = name; state = false; outputs = destinations } |> Some
      | "&" -> Conjunction { inputStates = Map.empty; name = name; outputs = destinations } |> Some
      | _ -> None

    let parse =
        Regex.groups "^(%|&)?([a-z]+) -> (.+)$"
        >> Seq.toArray
        >> function
            | [| _; moduleType; name; destinations |] -> buildModule moduleType name destinations
            | _ -> None

    Seq.choose parse


let one lines =
    parse lines |> printfn "%A"
    0
