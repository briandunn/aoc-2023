// For more information see https://aka.ms/fsharp-console-apps

module One =
    let one () =
        let lines = System.IO.File.ReadAllLines("./day-1-input.txt")

        let re = System.Text.RegularExpressions.Regex("\d")

        let fold sum line =
            let matches = re.Matches(line)

            let digits =
                [ Seq.tryHead; Seq.tryLast ]
                |> List.choose ((|>) matches)
                |> List.map (fun m -> m.Value)

            sum
            + System.Convert.ToInt32(String.concat "" digits)

        lines |> Array.fold fold 0 |> printfn "%d"

    let two () =
        let lastIndexOf (sub: string) (line: string) = 
            line.LastIndexOf(sub)

        let firstIndexOf (sub: string) (line: string) = 
            line.IndexOf(sub)

        let numberNames =
            [ "one"
              "two"
              "three"
              "four"
              "five"
              "six"
              "seven"
              "eight"
              "nine" ]

        let lines = System.IO.File.ReadAllLines("./day-1-input.txt")

        let subs =
            List.mapi (fun i name -> name, sprintf "%d" (i + 1)) numberNames
            @ ([ for i in 1..9 -> sprintf "%d" i ]
               |> List.map (fun n -> n, n))

        let fold sum (line: string) =
            let ((first, _), (last, _)) =
                let fold ((((_, firstIndex) as first), ((_, lastIndex) as last)) as acc) ((sub: string), n) =
                    match firstIndexOf sub line with
                    | -1 -> acc
                    | fi ->
                        let first =
                            if fi < firstIndex then
                                (n, fi)
                            else
                                first

                        let li = lastIndexOf sub line
                        let last = if li > lastIndex then (n, li) else last

                        first, last

                List.fold fold (("", String.length line), ("", -1)) subs

            let value =
                [ first; last ]
                |> String.concat ""
                |> System.Convert.ToInt32

            sum + value

        lines |> Array.fold fold 0 |> printfn "%d"


[<EntryPoint>]
let main args =
    match args with
    | [| "1"; "1" |] -> One.one ()
    | [| "1"; "2" |] -> One.two () // 54591
    | _ -> printfn "which puzzle?"

    0
