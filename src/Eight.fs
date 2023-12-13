module Eight

type Instruction =
    | R
    | L

type Node = string * string

type Network =
    { nodes: Map<string, Node>
      instructions: Instruction seq }

let parse: string seq -> Network =
    let parseInstructions: string -> Instruction seq =
        Seq.choose (function
            | 'R' -> Some R
            | 'L' -> Some L
            | _ -> None)

    let parseNodes: string seq -> Map<string, Node> =
        let labelRegex = "([A-Z0-9]{3})"

        let parseNode =
            Regex.groups (sprintf "^%s = \(%s, %s\)$" labelRegex labelRegex labelRegex)
            >> Seq.toList
            >> (function
            | [ _; label; l; r ] -> Some(label, (l, r))
            | _ -> None)

        Seq.choose parseNode >> Map.ofSeq

    Seq.toList
    >> function
        | instructions :: nodes ->
            { instructions = parseInstructions instructions
              nodes = parseNodes nodes }

        | _ ->
            { nodes = Map.empty
              instructions = Seq.empty }

let infinitely instructions =
    let length = Seq.length instructions
    let a = Seq.toArray instructions
    let initInfinite index = length |> ((%) index) |> Array.get a
    Seq.initInfinite initInfinite

let walk
    start
    { instructions = instructions
      nodes = nodes }
    =
    let scan (current: string) (instruction: Instruction) : string =
        match Map.find current nodes, instruction with
        | (l, r), R -> r
        | (l, r), L -> l

    instructions |> infinitely |> Seq.scan scan start

let rec gcd a b =
    match (a, b) with
    | (x, 0) -> x
    | (0, y) -> y
    | (a, b) -> gcd b (a % b)

let one: string seq -> int =
    parse
    >> walk "AAA"
    >> Seq.takeWhile ((<>) "ZZZ")
    >> Seq.length

let two lines =
    let { nodes = nodes } as network =
        parse lines

    let endsWith c = Seq.last >> ((=) c)

    let walk start = walk start network

    let stepCounts =
        nodes
        |> Map.keys
        |> Seq.filter (endsWith 'A')
        |> Seq.map (walk >> (Seq.takeWhile ((endsWith 'Z') >> not)) >> Seq.length)

    let gcd = Seq.reduce gcd stepCounts

    let fold acc stepCount =
      acc * int64 (stepCount / gcd)

    stepCounts
    |> Seq.fold fold (int64 gcd)
    |> printfn "%A"

    0
