module Thirteen

type Coords = int * int

type Pattern = Coords Set

let parse: string seq -> Pattern list =
    let fold patterns line =
        match patterns with
        | [] when line <> "" -> [ line ] :: patterns
        | head :: rest when line <> "" -> (line :: head) :: rest
        | _ :: _ when line = "" -> [] :: patterns
        | _ -> patterns

    let toPattern lines =
        Set.ofSeq
        <| seq {
            for y, line in List.indexed lines do
                for x, c in Seq.indexed line do
                    if c = '#' then yield x, y
        }

    Seq.fold fold []
    >> List.rev
    >> List.map (List.rev >> toPattern)

let toAxis groupBy map =
    Seq.groupBy groupBy
    >> Seq.sortBy fst
    >> Seq.map (snd >> Seq.map map >> Seq.toList)
    >> Seq.toList

let rows: Pattern -> int list list = toAxis snd fst

let columns: Pattern -> int list list = toAxis fst snd

let findSymmetry: int list list -> int =
    let rec findSymmetry prev =
        function
        | a :: (_ :: _ as rest) when Seq.forall2 (=) (a :: prev) rest -> List.length prev + 1
        | a :: (_ :: _ as rest) -> findSymmetry (a :: prev) rest
        | _ -> 0

    findSymmetry []


let one lines =
    let fold ((c, r): int * int) (pattern: Pattern) =
        c + (pattern |> columns |> findSymmetry), r + (pattern |> rows |> findSymmetry)

    let (columns, rows) = lines |> parse |> List.fold fold (0, 0)

    columns + (100 * rows)
