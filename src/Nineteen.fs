module Nineteen

type Op =
    | LT
    | GT

type Dest =
    | Jump of string
    | Accept
    | Reject

type Category =
    | X
    | M
    | A
    | S

type Rule =
    { category: Category
      value: int
      op: Op
      dest: Dest }

type Workflow =
    { name: string
      rules: Rule list
      otherwise: Dest }

let parse lines =
    let parseDest =
        function
        | "A" -> Accept
        | "R" -> Reject
        | label -> Jump label

    let parseCategory =
        function
        | "x" -> Some X
        | "m" -> Some M
        | "a" -> Some A
        | "s" -> Some S
        | _ -> None

    let parseRule =
        Regex.groups "^([xmas])([<>])(\d+):(.+)$"
        >> Seq.toArray
        >> function
            | [| _; category; op; value; dest |] ->
                let op =
                    match op with
                    | "<" -> Some LT
                    | ">" -> Some GT
                    | _ -> None

                let map2 category op =
                    { category = category
                      value = int value
                      op = op
                      dest = parseDest dest }

                Option.map2 map2 (parseCategory category) op
            | _ -> None

    let parseRules =
        String.split ','
        >> Seq.toList
        >> List.rev
        >> function
            | otherwise :: rules -> Some(rules |> List.rev |> List.choose parseRule, parseDest otherwise)
            | _ -> None

    let parseWorkflow: string -> Workflow option =
        Regex.groups "^([^\{]+)\{([^\}]+)\}$"
        >> Seq.toArray
        >> function
            | [| _; name; rules |] ->
                let map (rules, otherwise) =
                    { name = name
                      rules = rules
                      otherwise = otherwise }

                rules |> parseRules |> Option.map map
            | _ -> None

    let parseValue =
        Regex.groups "^([xmas])=(\d+)$"
        >> Seq.toArray
        >> function
            | [| _; category; value |] -> Option.map (fun category -> category, int value) (parseCategory category)
            | _ -> None

    let parsePart: string -> Map<Category, int> option =
        Regex.groups "^\{(.+)\}$"
        >> Seq.toArray
        >> function
            | [| _; values |] ->
                values
                |> String.split ','
                |> Seq.choose parseValue
                |> Map.ofSeq
                |> Some
            | _ -> None


    let workflows =
        lines
        |> Seq.takeWhile ((<>) "")
        |> Seq.choose parseWorkflow //, parts

    let parts =
        lines
        |> Seq.skipWhile ((<>) "")
        |> Seq.skip 1
        |> Seq.choose parsePart

    workflows, parts

let applyWorkflow workflow part =
    let applyRule rule =
        let value = Map.find rule.category part

        match rule.op with
        | LT when value < rule.value -> Some rule.dest
        | GT when value > rule.value -> Some rule.dest
        | _ -> None

    let rec applyRules =
        function
        | [] -> workflow.otherwise
        | head :: rest ->
            match applyRule head with
            | Some dest -> dest
            | None -> applyRules rest

    applyRules workflow.rules

let rec isAccepted start workflows part =
    let workflow = Map.find start workflows

    match applyWorkflow workflow part with
    | Accept -> true
    | Reject -> false
    | Jump label -> isAccepted label workflows part

let one lines =
    let workflows, parts = lines |> Seq.toArray |> parse

    let jumpTargets =
        workflows
        |> Seq.map (fun workflow ->
            (workflow.otherwise
             :: (List.map (fun ({ dest = dest }) -> dest) workflow.rules))
            |> List.choose (function
                | Jump label -> Some label
                | _ -> None))
        |> Seq.concat

    workflows
    |> Seq.filter (fun ({ name = name }) -> not (jumpTargets |> Seq.contains name))
    |> Seq.tryExactlyOne
    |> function
        | Some ({ name = name }) ->
            parts
            |> Seq.filter (
                isAccepted name (Map.ofSeq (Seq.map (fun ({ name = name } as workflow) -> name, workflow) workflows))
            )
            |> Seq.sumBy (Map.values >> Seq.sum)
        | None -> failwith "no starting point"


let openRanges =
    [ X; M; A; S ]
    |> Seq.map (fun c -> c, (1, 4000))
    |> Map.ofSeq

let acceptableRanges workflow =
    let fold (current, accepted) rule =

        let apply inverted =
            let change op (min, max) =
                match op with
                | LT when min < rule.value -> (min, rule.value - 1)
                | GT when max > rule.value -> (rule.value + 1, max)
                | _ -> (min, max)

            let op =
                match inverted, rule.op with
                | true, LT -> GT
                | true, GT -> LT
                | _, op -> op

            Map.change rule.category (Option.map (change op)) current

        match rule.dest with
        | Accept -> openRanges, (apply false) :: accepted
        | _ -> (apply true), accepted

    let current, accepted = workflow.rules |> Seq.fold fold (openRanges, [])

    match workflow.otherwise with
    | Accept -> Some (workflow.name, current :: accepted)
    | _ when accepted <> [] -> Some (workflow.name, accepted)
    | _ -> None

let two lines =
    let workflows, _ = lines |> Seq.toArray |> parse

    workflows
    |> Seq.choose acceptableRanges
    |> Seq.iter (printfn "%A")

    0
