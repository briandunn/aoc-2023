// For more information see https://aka.ms/fsharp-console-apps

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

module Three =
    type Coords = int * int

    type Number =
        { coords: Coords
          length: int
          value: int }

    module Number =
        let value ({ value = value }) = value

    let grid lines =
        let lines = Array.map (fun (line: string) -> line.ToCharArray()) lines
        let initializer x y = Array.get (Array.get lines y) x
        Array2D.init (lines |> Array.map Array.length |> Array.max) (Array.length lines) initializer

    let coords { coords = (x, y); length = length } =
        seq { for x in x .. x + length - 1 -> x, y }

    let surroundingCoords ({ coords = (x, y); length = length } as number) grid =
        seq {
            for y in y - 1 .. y + 1 do
                if y >= 0 && y < Array2D.length2 grid then
                    for x in x - 1 .. x + length do
                        if x >= 0 && x < Array2D.length1 grid then
                            x, y
        }
        |> Seq.except (coords number)

    let numbers lines =
        let digits = new System.Text.RegularExpressions.Regex("\d+")

        seq {
            for (y, line) in Array.indexed lines do
                for m in digits.Matches(line) do
                    { coords = m.Index, y
                      length = m.Length
                      value = System.Convert.ToInt32(m.Value) }
        }


    let one lines =
        let grid = grid lines
        let numbers = numbers lines

        let surrounding number =
            surroundingCoords number grid
            |> Seq.map (fun (x, y) -> Array2D.get grid x y)

        let containsSymbol = Seq.filter ((<>) '.') >> Seq.isEmpty >> not

        let fold acc number =
            if number |> surrounding |> containsSymbol then
                acc + number.value
            else
                acc

        numbers |> Seq.fold fold 0 |> printfn "%A"

    let two lines =
        let grid = grid lines
        let numbers = numbers lines

        let gears =
            seq {
                for x in 0 .. Array2D.length1 grid - 1 do
                    for y in 0 .. Array2D.length2 grid - 1 do
                        if Array2D.get grid x y = '*' then x, y
            }

        let choose gear =
            let filter number =
                surroundingCoords number grid |> Seq.contains gear

            let touching = Seq.filter filter numbers

            if Seq.length touching = 2 then
                Some touching
            else
                None

        let gearRatio: Number seq -> int =
            let fold acc ({ value = value }) = acc * value
            Seq.fold fold 1

        gears
        |> Seq.choose choose
        |> Seq.sumBy gearRatio
        |> printfn "%A"


module Four =
    type Card =
        { id: int
          have: int Set
          winning: int Set }

    let cardRe =
        new System.Text.RegularExpressions.Regex("^Card\s+(\d+):\s+([^\|]+)\|(.*)$")

    let numRe = new System.Text.RegularExpressions.Regex("\d+")

    let parseCard line =
        let toSet (m: System.Text.RegularExpressions.Group) =
            let map (v: System.Text.RegularExpressions.Match) = System.Convert.ToInt32(v.Value)

            numRe.Matches(m.Value) |> Seq.map map |> Set.ofSeq

        match cardRe.Match(line).Groups |> List.ofSeq with
        | [ _; id; winning; have ] ->
            Some
                { id = System.Convert.ToInt32(id.Value)
                  have = toSet have
                  winning = toSet winning }
        | _ -> None

    let matchCount ({ have = have; winning = winning }) =
        winning |> Set.intersect have |> Seq.length

    let one =
        let score card = pown 2 ((matchCount card) - 1)

        Seq.choose parseCard
        >> Seq.map score
        >> Seq.sum
        >> printfn "%A"

    let two =
        let score card = matchCount card, 1
        let copies (_score, copies) = copies

        let rec loop =
            function
            | (score, copies) as head :: tail ->
                let inc (score, c) = score, c + copies
                let take = tail |> List.length |> min score

                head
                :: loop (
                    (tail |> List.take take |> List.map inc)
                    @ (List.skip take tail)
                )

            | [] -> []

        Seq.choose parseCard
        >> Seq.map score
        >> Seq.toList
        >> loop
        >> List.sumBy copies
        >> printfn "%A"

[<EntryPoint>]
let main args =
    let readInput day =
        System.IO.File.ReadAllLines(sprintf "./day-%s-input.txt" day)

    match args with
    | [| "1"; "1" |] -> "1" |> readInput |> One.one
    | [| "1"; "2" |] -> "1" |> readInput |> One.two
    | [| "2"; "1" |] -> "2" |> readInput |> Two.one
    | [| "2"; "2" |] -> "2" |> readInput |> Two.two
    | [| "3"; "1" |] -> "3" |> readInput |> Three.one
    | [| "3"; "2" |] -> "3" |> readInput |> Three.two // 87287096
    | [| "4"; "1" |] -> "4" |> readInput |> Four.one
    | [| "4"; "2" |] -> "4" |> readInput |> Four.two // 6189740
    | _ -> printfn "which puzzle?"

    0
