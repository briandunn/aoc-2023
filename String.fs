module String
  open System
  open System.Text.RegularExpressions

  let numRe = new Regex("\d+")

  let parseNumbers (line: string) : int64 seq =
      let map (v: Match) =
          try
              Convert.ToInt64(v.Value)
          with
          | :? OverflowException ->
              printfn "overflow: %s" v.Value
              0


      Seq.map map <| numRe.Matches(line)


  let parseInts (line: string) : int seq =
      let map (v: Match) = (int v.Value)

      Seq.map map <| numRe.Matches(line)
