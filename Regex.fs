module Regex

open System.Text.RegularExpressions

let groups (input: string) (expression: string) =
    (new Regex(expression)).Match(input).Groups
    |> Seq.map (fun g -> g.Value)
