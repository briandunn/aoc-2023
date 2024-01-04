module Twenty

type Level =
    | High
    | Low

type FlipFlop = { state: bool }

type Conjunction = { inputStates: Map<string, Level> }

type ModuleState =
    | FlipFlop of FlipFlop
    | Conjunction of Conjunction
    | Broadcaster

type Module =
    { name: string
      outputs: string list
      state: ModuleState }

type Pulse =
    { src: string
      dest: string
      level: Level }

let parse: string seq -> Module seq =
    let buildModule name destinations =
        let destinations =
            destinations
            |> Regex.matches "([a-z]+)"
            |> Seq.toList

        function
        | "" -> Broadcaster |> Some
        | "%" -> { state = false } |> FlipFlop |> Some
        | "&" -> { inputStates = Map.empty } |> Conjunction |> Some
        | _ -> None
        >> Option.map (fun state ->
            { name = name
              outputs = destinations
              state = state })

    let parse =
        Regex.groups "^(%|&)?([a-z]+) -> (.+)$"
        >> Seq.toArray
        >> function
            | [| _; moduleType; name; destinations |] -> buildModule name destinations moduleType
            | _ -> None

    Seq.choose parse

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
    | Broadcaster, { level = Low } -> Some(Low, m)
    | Broadcaster, _ -> None
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

let pushButton =
    let rec loop history (modules: Map<string, Module>) =
        function
        | [] -> history, modules
        | { dest = dest } as head :: rest ->
            modules
            |> Map.tryFind dest
            |> Option.bind (receive head)
            |> function
                | None -> loop (head :: history) modules rest
                | Some (level, m) -> loop (head :: history) (Map.add dest m modules) (rest @ (send m level))

    loop []

let level { level = level } = level
let one lines =
    let modules =
        seq { for m in parse lines -> m.name, m }
        |> Map.ofSeq
        |> initializeConjunctions

    let scan (_, modules) _ =
        pushButton
            modules
            [ { src = "button"
                dest = "broadcaster"
                level = Low } ]

    [ 1..1000 ]
    |> Seq.scan scan ([], modules)
    |> Seq.map fst
    |> Seq.concat
    |> Seq.countBy level
    |> Ten.p "levels"
    |> Seq.map snd
    |> Seq.reduce (*)
