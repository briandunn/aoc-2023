module Seven

type Hand = int list

type Turn = { hand: Hand; bid: int }

let strengths =
    [ 'A'
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
    |> List.rev
    |> List.mapi (fun i c -> (c, i))
    |> Map.ofList

let parse lines =
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

let one lines =
    let sortWith { hand = handA } { hand = handB } =
        match compare (handType handA) (handType handB) with
        | 0 -> compare handA handB
        | x -> x

    let mapi i { bid = bid } = (i + 1) * bid

    parse lines
    |> Seq.sortWith sortWith
    |> Seq.mapi mapi
    |> Seq.sum
