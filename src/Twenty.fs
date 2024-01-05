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

let pushButton modules =
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


    loop [] modules
    >> function
        | history, modules -> (List.rev history), modules

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
    |> Seq.map snd
    |> Seq.reduce (*)

let run modules =
    let scan (_, _, modules) i =
        let pulses, modules =
            pushButton
                modules
                [ { src = "button"
                    dest = "broadcaster"
                    level = Low } ]

        i, pulses, modules

    Seq.initInfinite id
    |> Seq.scan scan (0, [], modules)
    |> Seq.tail


let partition modules =
    let rec partition visited =
        function
        | [] -> visited
        | head :: rest ->
            Map.tryFind head modules
            |> function
                | Some { outputs = outputs } ->
                    visited
                    |> Set.difference (Set.ofList outputs)
                    |> Set.toList
                    |> List.append rest
                    |> partition (Set.add head visited)
                | None ->
                    partition visited rest

    let slice names =
        seq { for name in names -> name, Map.find name modules }
        |> Map.ofSeq

    List.singleton >> partition Set.empty >> slice

let partitionBroadcaster (modules: Module seq) =
    modules
    |> Seq.tryFind (function
        | { state = Broadcaster } -> true
        | _ -> false)
    |> function
        | Some { outputs = outputs } ->
            let modules = seq { for m in modules -> m.name, m } |> Map.ofSeq

            let map output =
                (partition modules output)
                |> Map.add
                    "broadcaster"
                    { name = "broadcaster"
                      outputs = [ output ]
                      state = Broadcaster }

            outputs |> List.map map
        | None -> []

let toGraph pre modules =
    let nodes =
        seq {
            for m in modules ->
                match m.state with
                | Broadcaster -> sprintf "%s [shape=box]" m.name
                | FlipFlop _ -> sprintf "%s [shape=ellipse]" m.name
                | Conjunction _ -> sprintf "%s [shape=diamond]" m.name
        }

    let connections =
        seq {
            for { name = name; outputs = outputs } in modules do
                for output in outputs -> sprintf "%s -> %s" name output
        }

    nodes
    |> Seq.append connections
    |> String.concat ";\n"
    |> sprintf "subgraph {\n%s\n;%s;\n}" pre

let graphPartitions lines =
    let mapi i m =
        let color = "red green blue brown" |> String.split ' ' |> Seq.item (i % 4)
        m |> Map.values |> toGraph (sprintf "edge [color=\"%s\"]" color)

    parse lines
    |> Seq.toList
    |> partitionBroadcaster
    |> Seq.mapi mapi
    |> String.concat "\n"
    |> printfn "digraph {\n%s;\n}"

    0

let two lines =
    let modules = parse lines |> Seq.toList

    let partitions =
        modules
        |> Seq.tryFind (function
            | { state = Broadcaster } -> true
            | _ -> false)
        |> function
            | Some { outputs = outputs } ->
                let modules = seq { for m in modules -> m.name, m } |> Map.ofSeq

                let map output =
                    (partition modules output)
                    |> Map.add
                        "broadcaster"
                        { name = "broadcaster"
                          outputs = [ output ]
                          state = Broadcaster }

                outputs |> List.map map
            | None -> []


    let mapi i m =
        m |> Map.find "broadcaster",
        m
        |> (initializeConjunctions
            >> run
            >> Seq.take 10_000
            >> Seq.map (fun (_, pulses, _) -> pulses)
            >> Seq.concat
            >> Seq.filter (function
                | { dest = "rx" } -> true
                | _ -> false)
            >> Seq.countBy (fun { level = level } -> level)
            >> Seq.toList)

    partitions
    |> Seq.mapi (
        mapi
    // initializeConjunctions
    // >> run
    // >> Seq.take 10_000
    // >> Seq.map (fun (_, pulses, _) -> pulses)
    // >> Seq.concat
    // >> Seq.countBy id
    // >> Seq.toList
    // >> Seq.takeWhile (function
    //     | i, pulses, _modules ->
    //         pulses
    //         |> List.exists (function
    //             | { dest = "rx"; level = Low } -> true
    //             | _ -> false)
    //         |> not)
    // >> Seq.last
    )
    |> printfn "%A"

    0
