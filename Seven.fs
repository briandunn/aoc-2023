module Seven

type Hand = int list

type Turn = { hand: Hand; bid: int }

let strengthMap: char list -> Map<char, int> =
    List.rev
    >> List.mapi (fun i c -> (c, i))
    >> Map.ofList

let parse strengths lines =
    let strengths = strengthMap strengths

    let parseTurn line =
        line
        |> Regex.groups "([AKQJT98765432]+) ([0-9]+)"
        |> Seq.toList
        |> function
            | [ _; hand; bid ] ->
                Some(
                    { hand =
                        (hand.ToCharArray())
                        |> Seq.choose (fun c -> Map.tryFind c strengths)
                        |> Seq.toList
                      bid = int bid }
                )
            | _ -> None

    lines |> Seq.choose parseTurn

let strengthCounts: Hand -> int list =
    List.countBy id
    >> List.map Tuple.second
    >> List.sort

let fiveOfAKind: Hand -> bool = Set.ofList >> Set.count >> (=) 1

let fourOfAKind: Hand -> bool = strengthCounts >> Seq.contains 4

let fullHouse: Hand -> bool = strengthCounts >> ((=) [ 2; 3 ])

let threeOfAKind: Hand -> bool = strengthCounts >> ((=) [ 1; 1; 3 ])

let twoPair: Hand -> bool = strengthCounts >> ((=) [ 1; 2; 2 ])

let onePair: Hand -> bool = strengthCounts >> ((=) [ 1; 1; 1; 2 ])

let handTypes =
    [ fiveOfAKind
      fourOfAKind
      fullHouse
      threeOfAKind
      twoPair
      onePair ]
    |> List.rev
    |> List.indexed

let handType hand =
    let tryFind (i, f) = f hand

    handTypes
    |> List.tryFind tryFind
    |> Option.map (fun (i, _) -> i + 1)
    |> Option.defaultValue 0

let totalWinnings handType =
    let sortWith { hand = handA } { hand = handB } =
        match compare (handType handA) (handType handB) with
        | 0 -> compare handA handB
        | x -> x

    let mapi i { bid = bid } = (i + 1) * bid

    Seq.sortWith sortWith >> Seq.mapi mapi >> Seq.sum

let one: string seq -> int =
    parse [ 'A'
            'K'
            'Q'
            'J'
            'T'
            '9'
            '8'
            '7'
            '6'
            '5'
            '4'
            '3'
            '2' ]
    >> totalWinnings handType

let two: string seq -> int =
    let strengths =
        [ 'A'
          'K'
          'Q'
          'T'
          '9'
          '8'
          '7'
          '6'
          '5'
          '4'
          '3'
          '2'
          'J' ]

    let rec expandJokers jokerCount hand =
        match jokerCount with
        | 0 -> [ hand ]
        | count ->
            [ for s in 0 .. (List.length strengths - 1) -> (s :: hand) ]
            |> List.map (expandJokers (count - 1))
            |> List.concat

    let bestJokers: Hand -> int =
        List.partition ((=) 0)
        >> function
            | ([], hand) -> handType hand
            | (jokers, hand) ->
                hand
                |> expandJokers (List.length jokers)
                |> List.map handType
                |> List.max

    parse strengths >> totalWinnings bestJokers
