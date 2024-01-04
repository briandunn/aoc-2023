module Twenty

type Broadcaster = string list

type Level =
    | High
    | Low

type FlipFlop = { state: bool }

type Conjunction = { inputStates: Map<string, Level> }

type ModuleState =
    | FlipFlop of FlipFlop
    | Conjunction of Conjunction

type Module =
    { name: string
      outputs: string list
      state: ModuleState }

type InputLine =
    | Broadcaster of Broadcaster
    | Module of Module

type Pulse =
    { src: string
      dest: string
      level: Level }

let parse: string seq -> Broadcaster * Map<string, Module> =
    let buildModule moduleType name destinations =
        let destinations =
            destinations
            |> Regex.matches "([a-z]+)"
            |> Seq.toList

        match moduleType with
        | "" -> destinations |> Broadcaster |> Some
        | "%" ->
            { name = name
              outputs = destinations
              state = FlipFlop { state = false } }
            |> Module
            |> Some
        | "&" ->
            { name = name
              outputs = destinations
              state = Conjunction { inputStates = Map.empty } }
            |> Module
            |> Some
        | _ -> None

    let parse =
        Regex.groups "^(%|&)?([a-z]+) -> (.+)$"
        >> Seq.toArray
        >> function
            | [| _; moduleType; name; destinations |] -> buildModule moduleType name destinations
            | _ -> None

    let fold (broadcaster, modMap) =
        function
        | Broadcaster destinations -> (Some destinations, modMap)
        | Module ({ name = name } as m) -> broadcaster, (Map.add name m modMap)

    Seq.choose parse
    >> Seq.fold fold (None, Map.empty)
    >> function
        | Some broadcaster, modMap -> broadcaster, modMap
        | None, _ -> failwith "No broadcaster found"

let initializeConjunctions modules =
    let conjunctions =
        let choose (name, m) =
            match m.state with
            | Conjunction _ -> Some(name, [])
            | _ -> None

        modules
        |> Map.toSeq
        |> Seq.choose choose
        |> Map.ofSeq

    let conjunctionInputs =
        let fold conjunctions (name, { outputs = outputs }) =
            let fold conjunctions output =
                match Map.tryFind output conjunctions with
                | Some outputs -> Map.add output (name :: outputs) conjunctions
                | None -> conjunctions

            outputs |> List.fold fold conjunctions

        modules |> Map.toSeq |> Seq.fold fold conjunctions

    let fold acc name inputs =
        let change: Module -> Module =
            function
            | { state = Conjunction _ } as m ->
                { m with state = Conjunction { inputStates = [ for i in inputs -> i, Low ] |> Map.ofList } }
            | m -> m

        Map.change name (Option.map change) acc

    conjunctionInputs |> Map.fold fold modules

let send m level =
    [ for dest in m.outputs ->
          { src = m.name
            level = level
            dest = dest } ]

let receive pulse m : (Level * Module) option =
    match m.state, pulse with
    | FlipFlop _, { level = High } -> None
    | FlipFlop { state = state }, _ ->
        Some((if state then Low else High), { m with state = FlipFlop { state = not state } })

    | Conjunction { inputStates = inputStates }, { src = src; level = pulse } ->
        let inputStates = inputStates |> Map.add src pulse

        let outPulse =
            if inputStates |> Map.values |> Seq.forall ((=) High) then
                Low
            else
                High

        Some(outPulse, { m with state = Conjunction { inputStates = inputStates } })

let pushButton broadcaster modules =
    let rec loop history (modules: Map<string, Module>) pulses =
        match pulses with
        | [] -> history, modules
        | { dest = dest } as head :: rest ->
            modules
            |> Map.tryFind dest
            |> Option.bind (receive head)
            |> function
                | None -> loop (head :: history) modules rest
                | Some (level, m) -> loop (head :: history) (Map.add dest m modules) (rest @ (send m level))

    [ for output in broadcaster ->
          { dest = output
            src = "broadcaster"
            level = Low } ]
    |> loop [] modules

let one lines =
    let broadcaster, modules = parse lines

    let modules = initializeConjunctions modules

    pushButton broadcaster modules |> printfn "%A"

    0
