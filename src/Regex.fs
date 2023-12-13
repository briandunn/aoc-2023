module Regex

open System.Text.RegularExpressions

let groups (expression: string) (input: string) =
    (new Regex(expression)).Match(input).Groups
    |> Seq.map (fun g -> g.Value)

let matches (expression: string) (input: string) =
    (new Regex(expression)).Matches(input)
    |> Seq.map (fun m -> m.Value)