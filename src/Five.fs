module Five

#nowarn "40"

open System.Text.RegularExpressions

type Range =
    { sourceStart: int64
      destStart: int64
      length: int64 }

type RangeMap =
    { source: string
      dest: string
      ranges: Range list }

type Almanac =
    { inputs: int64 list
      maps: RangeMap list }

module RangeMap =
    let sort (map: Map<string, RangeMap>) =
        let rec loop rangeMap : RangeMap list =
            rangeMap
            :: (map
                |> Map.tryFind rangeMap.dest
                |> Option.map loop
                |> Option.defaultValue [])

        map
        |> Map.tryFind "seed"
        |> Option.map loop
        |> Option.defaultValue []

let parse: string seq -> Almanac =
    let rec parseRanges =
        function
        | head :: rest ->
            match head |> String.parseNumbers |> Seq.toList with
            | [ destStart; sourceStart; length ] ->
                let ranges, rest = parseRanges rest

                { sourceStart = sourceStart
                  destStart = destStart
                  length = length }
                :: ranges,
                rest
            | _ -> [], head :: rest
        | [] -> [], []

    let rec parseMaps: string list -> RangeMap list * string list =
        let headerRe = Regex("^([^\-]+)\-to\-(\S+) map:$")

        function
        | "" :: rest -> parseMaps rest
        | header :: rest when headerRe.IsMatch header ->
            "^([^\-]+)\-to\-(\S+) map:$"
            |> Regex.groups header
            |> Seq.toList
            |> function
                | [ _; source; dest ] ->
                    let ranges, rest = parseRanges rest
                    let maps, rest = parseMaps rest

                    { source = source
                      dest = dest
                      ranges = ranges }
                    :: maps,
                    rest
                | _ -> [], []
        | _ -> [], []


    Seq.toList
    >> function
        | inputs :: rest ->
            let maps, _ = parseMaps rest

            { inputs = inputs |> String.parseNumbers |> Seq.toList
              maps = maps }
        | _ -> { inputs = []; maps = [] }

let resolveForwards maps =
    let maps =
        maps
        |> List.map (fun m -> m.source, m)
        |> Map.ofList

    let resolveRange
        input
        { sourceStart = sourceStart
          destStart = destStart
          length = length }
        =
        if input >= sourceStart
           && input < (sourceStart + length) then
            Some(destStart + (input - sourceStart))
        else
            None

    let resolveMap input { dest = dest; ranges = ranges } =
        match ranges |> List.choose (resolveRange input) with
        | [] -> Some(dest, input)
        | [ mapped ] -> Some(dest, mapped)
        | x ->
            printfn "%A" (x, dest)
            None


    let rec resolveInput (kind: string) input =
        Map.tryFind kind maps
        |> Option.bind (resolveMap input)
        |> function
            | Some (dest, mapped) -> resolveInput dest mapped
            | None -> (kind, input)

    resolveInput "seed"

let resolveBackwards maps =
    let maps =
        maps
        |> List.map (fun m -> m.dest, m)
        |> Map.ofList

    let resolveRange
        input
        { sourceStart = sourceStart
          destStart = destStart
          length = length }
        =
        if input >= destStart && input < (destStart + length) then
            Some(sourceStart + (input - destStart))
        else
            None

    let resolveMap input { source = source; ranges = ranges } =
        match ranges |> List.choose (resolveRange input) with
        | [] -> Some(source, input)
        | [ mapped ] -> Some(source, mapped)
        | x ->
            printfn "%A" (x, source)
            None


    let rec resolveInput (kind: string) input =
        Map.tryFind kind maps
        |> Option.bind (resolveMap input)
        |> function
            | Some (dest, mapped) -> resolveInput dest mapped
            | None -> (kind, input)

    resolveInput "location"

let one lines =
    let { inputs = inputs; maps = maps } = parse lines

    inputs
    |> Seq.map (resolveForwards maps)
    |> Seq.minBy Tuple.second
    |> Tuple.second
    |> int


let two lines =
    let { inputs = inputs; maps = maps } = parse lines

    let rec expand =
        function
        | start :: length :: rest -> (start, length) :: expand rest
        | _ -> []

    let inputs = expand inputs

    let isInput (_kind, i) =
        let exists (start, length) = i >= start && i < (start + length)
        List.exists exists inputs

    let minOut =
        maps
        |> List.map (fun m -> m.ranges)
        |> List.concat
        |> List.map (fun r -> r.destStart)
        |> List.min

    Seq.initInfinite (int64 >> ((+) minOut))
    |> Seq.filter ((resolveBackwards maps) >> isInput)
    |> Seq.head
    |> int
