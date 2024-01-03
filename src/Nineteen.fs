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

type Part = Map<Category, int>

type PartRange = Map<Category, int * int>

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


let openPartRange: PartRange =
    [ X; M; A; S ]
    |> Seq.map (fun c -> c, (1, 4000))
    |> Map.ofSeq

let countPermutations partRange =
    let map category =
        let (start, stop) = Map.find category partRange
        (stop - start) + 1 |> bigint

    [ X; M; A; S ] |> List.map map |> List.reduce (*)

let adjustRanges rule partRange inverted =
    let change (min, max) =
        match rule.op, inverted with
        | LT, false when min < rule.value -> (min, rule.value - 1)
        | GT, false when max > rule.value -> (rule.value + 1, max)
        | LT, true when min < rule.value -> (rule.value, max)
        | GT, true when max > rule.value -> (min, rule.value)
        | _ -> (min, max)

    Map.change rule.category (Option.map change) partRange

let totalCombinations workflows =
    let rec totalCombinations partRange =
        let follow partRange =
            function
            | Accept -> countPermutations partRange
            | Reject -> bigint 0
            | Jump name -> totalCombinations partRange (Map.tryFind name workflows)

        let applyRule rule workflow =
            totalCombinations (adjustRanges rule partRange true) (Some workflow)
            + follow (adjustRanges rule partRange false) rule.dest

        function
        | Some ({ rules = []; otherwise = otherwise }) -> follow partRange otherwise
        | Some ({ rules = rule :: rest } as workflow) -> applyRule rule { workflow with rules = rest }
        | None -> bigint 0

    totalCombinations


let two lines =
    let workflows, _ = lines |> Seq.toArray |> parse

    let workflowMap =
        workflows
        |> Seq.map (fun wf -> wf.name, wf)
        |> Map.ofSeq

    workflowMap
    |> Map.tryFind "in"
    |> totalCombinations workflowMap openPartRange
    |> printfn "%A"

    0
