// For more information see https://aka.ms/fsharp-console-apps

module One =
    let one =

        let re = System.Text.RegularExpressions.Regex("\d")

        let fold sum line =
            let matches = re.Matches(line)

            let digits =
                [ Seq.tryHead; Seq.tryLast ]
                |> List.choose ((|>) matches)
                |> List.map (fun m -> m.Value)

            sum
            + System.Convert.ToInt32(String.concat "" digits)

        Seq.fold fold 0

    let two =
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

        Seq.fold fold 0

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

        "^Game\s+(\d+):\s+(.*)$"
        |> Regex.groups line
        |> Seq.toList
        |> function
            | [ _; gameId; hands ] ->
                Some
                    { id = (int) gameId
                      hands = parseHands hands }
            | _ -> None

    let games = Seq.choose parseGame >> Seq.toList

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

        games >> List.filter filter >> List.sumBy sumBy

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

        games >> List.map (fewest >> power) >> List.sum

module Three =
    type Coords = int * int

    type Number =
        { coords: Coords
          length: int
          value: int }

    module Number =
        let value ({ value = value }) = value

    let grid lines =
        let lines = Seq.map (fun (line: string) -> line.ToCharArray()) lines
        let initializer x y = lines |> Seq.item y |> Seq.item x
        Array2D.init (lines |> Seq.map Array.length |> Seq.max) (Seq.length lines) initializer

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
            for (y, line) in Seq.indexed lines do
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

        numbers |> Seq.fold fold 0

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

        gears |> Seq.choose choose |> Seq.sumBy gearRatio

module Four =
    type Card =
        { id: int
          have: int64 Set
          winning: int64 Set }

    let parseCard line =
        let toSet = String.parseNumbers >> Set.ofSeq

        line
        |> Regex.groups "^Card\s+(\d+):\s+([^\|]+)\|(.*)$"
        |> Seq.toList
        |> function
            | [ _; id; winning; have ] ->
                Some
                    { id = int id
                      have = toSet have
                      winning = toSet winning }
            | _ -> None

    let matchCount ({ have = have; winning = winning }) =
        winning |> Set.intersect have |> Seq.length

    let one =
        let score card = pown 2 ((matchCount card) - 1)

        Seq.choose parseCard >> Seq.sumBy score

    let two =
        let score card = matchCount card, 1
        let copies (_score, copies) = copies

        let rec loop =
            function
            | [] -> []
            | (score, copies) as head :: tail ->
                let inc (score, c) = score, c + copies
                let toDup, tail = List.splitAt (tail |> List.length |> min score) tail
                head :: loop ((List.map inc toDup) @ tail)

        Seq.choose parseCard
        >> Seq.map score
        >> Seq.toList
        >> loop
        >> List.sumBy copies

[<EntryPoint>]
let main args =

    let readLines (stream: System.IO.TextReader) =
        Seq.initInfinite (fun _ -> stream.ReadLine())
        |> Seq.takeWhile ((<>) null)

    let puzzles =
        seq {
            for day, puzzle, f in
                [ 1, 1, One.one
                  1, 2, One.two
                  2, 1, Two.one
                  2, 2, Two.two
                  3, 1, Three.one
                  3, 2, Three.two
                  4, 1, Four.one
                  4, 2, Four.two
                  5, 1, Five.one
                  5, 2, Five.two
                  6, 1, Six.one
                  6, 2, Six.two
                  7, 1, Seven.one
                  7, 2, Seven.two ] -> (day, puzzle), f
        }
        |> Map.ofSeq

    args
    |> String.concat " "
    |> String.parseInts
    |> Seq.toList
    |> function
        | [ day; puzzle ] ->
            puzzles
            |> Map.tryFind (day, puzzle)
            |> function
                | Some f ->
                    stdin |> readLines |> f |> printfn "%A"
                    0
                | None ->
                    printfn "No puzzle %d for day %d" puzzle day
                    1


        | m ->
            printfn "invalid input: %A" m
            printfn "Usage: dotnet run <day> <puzzle> < input.txt"
            1
