
module Five
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
        maps: Map<string, RangeMap> }

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
              headerRe.Match(header).Groups
              |> Seq.toList
              |> function
                  | [ _; source; dest ] ->
                      let ranges, rest = parseRanges rest
                      let maps, rest = parseMaps rest

                      { source = source.Value
                        dest = dest.Value
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
                maps = Map.ofList [ for map in maps -> map.source, map ] }
          | _ -> { inputs = []; maps = Map.empty }

  let resolve maps =

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

      Seq.map (resolveInput "seed")

  let one lines =
      let { inputs = inputs; maps = maps } = parse lines

      inputs
      |> resolve maps
      |> Seq.minBy Tuple.second
      |> printfn "%A"


  let two lines =
      let { inputs = inputs; maps = maps } = parse lines

      let rec expand =
          function
          | start :: length :: rest -> seq { for i in start..start + length - 1L -> i } |> Seq.append (expand rest)
          | _ -> Seq.empty

      let inputs = expand inputs

      inputs
      |> resolve maps
      |> Seq.minBy Tuple.second
      |> printfn "%A"
