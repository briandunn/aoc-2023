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

let one lines =
    let workflows, parts = lines |> parse
    parts |> Seq.iter (printfn "%A")
    0
