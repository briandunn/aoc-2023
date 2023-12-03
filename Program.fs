﻿// For more information see https://aka.ms/fsharp-console-apps

module One =
    let one lines =

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

    let two lines =
        let lastIndexOf (sub: string) (line: string) = line.LastIndexOf(sub)

        let firstIndexOf (sub: string) (line: string) = line.IndexOf(sub)

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

module Tuple =
    let first (a, b) = a

module Two =
    type Color =
        | Red
        | Green
        | Blue

    type Hand = Map<Color, int>
    type Game = { id: int; hands: Hand list }

    let parseGame (line: string) =
        let parseHands (hands: string) : Hand list =
            let parseHand (hand: string) : Map<Color, int> =
                let map (color: string) =
                    let parseColor =
                        function
                        | "red" -> Some Red
                        | "green" -> Some Green
                        | "blue" -> Some Blue
                        | _ -> None

                    color.Split(" ")
                    |> function
                        | [| count; color |] ->
                            color
                            |> parseColor
                            |> Option.map (fun color -> (color, System.Convert.ToInt32 count))
                        | _ -> None

                hand.Split(", ") |> Seq.choose map |> Map.ofSeq

            hands.Split("; ")
            |> Seq.map parseHand
            |> Seq.toList

        let re = new System.Text.RegularExpressions.Regex("^Game\s+(\d+):\s+(.*)$")

        re.Match(line).Groups
        |> Seq.toList
        |> function
            | [ _; gameId; hands ] ->
                Some
                    { id = System.Convert.ToInt32 gameId.Value
                      hands = parseHands hands.Value }
            | _ -> None

    let games = Array.choose parseGame >> Array.toList

    let one =
        let maximums =
            Map.ofList [ Red, 12
                         Green, 13
                         Blue, 14 ]


        let handIsPossible (hand: Hand) : bool =
            let forall color =
                [ hand; maximums ]
                |> List.choose (Map.tryFind color)
                |> function
                    | [ g; m ] -> g <= m
                    | [ _m ] -> true
                    | _ -> false

            [ Red; Green; Blue ] |> List.forall forall

        let filter ({ hands = hands }) = List.forall handIsPossible hands

        let sumBy ({ id = id }) = id

        games
        >> List.filter filter
        >> List.sumBy sumBy
        >> printfn "%A"

    let two =
        let maxHand (a: Hand) : Hand -> Hand =
            let fold (acc: Hand) (color, count) : Hand =
                let newMax =
                    match Map.tryFind color acc with
                    | Some previousMax when count > previousMax -> count
                    | Some previousMax -> previousMax
                    | None -> count

                Map.add color newMax acc

            List.fold fold a << Map.toList

        let fewest ({ hands = hands }: Game) : Hand = List.fold maxHand Map.empty hands

        let power: Hand -> int = Map.values >> Seq.fold (*) 1

        games
        >> List.map (fewest >> power)
        >> List.sum
        >> printfn "%A"

let readInput day =
    System.IO.File.ReadAllLines(sprintf "./day-%s-input.txt" day)

// [| "467..114.."
//    "...*......"
//    "..35..633."
//    "......#..."
//    "617*......"
//    ".....+.58."
//    "..592....."
//    "......755."
//    "...$.*...."
//    ".664.598.." |]

module Three =

    let grid lines =
        let lines = Array.map (fun (line: string) -> line.ToCharArray()) lines
        let initializer x y = Array.get (Array.get lines y) x
        Array2D.init (lines |> Array.map Array.length |> Array.max) (Array.length lines) initializer

    let one lines =
        let digits = new System.Text.RegularExpressions.Regex("\d+")

        let grid = grid lines

        let surrounding y x length =
            seq {
                for y in y - 1 .. y + 1 do
                    if y >= 0 && y < Array2D.length2 grid then
                        for x in x - 1 .. x + length do
                            if x >= 0 && x < Array2D.length1 grid then
                                x, y
            }
            |> Seq.except (seq { for x in x .. x + length - 1 -> x, y })
            |> Seq.map (fun (x, y) -> Array2D.get grid x y)

        let containsSymbol = Seq.filter ((<>) '.') >> Seq.isEmpty >> not

        let map i line =
            seq {
                for m in digits.Matches(line) do
                    if containsSymbol <| surrounding i m.Index m.Length then
                        System.Convert.ToInt32(m.Value)
            }


        Seq.concat <| Array.mapi map lines
        |> Seq.sum
        |> printfn "%A"

    let two lines = ()


[<EntryPoint>]
let main args =
    match args with
    | [| "1"; "1" |] -> "1" |> readInput |> One.one
    | [| "1"; "2" |] -> "1" |> readInput |> One.two
    | [| "2"; "1" |] -> "2" |> readInput |> Two.one
    | [| "2"; "2" |] -> "2" |> readInput |> Two.two
    | [| "3"; "1" |] -> "3" |> readInput |> Three.one
    | [| "3"; "2" |] -> "3" |> readInput |> Three.two
    | _ -> printfn "which puzzle?"

    0
