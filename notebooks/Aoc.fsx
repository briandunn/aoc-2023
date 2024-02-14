module Aoc

let fetchInput day =
    let session =
        "53616c7465645f5f10b226a63ffd38a2e4e417f9686924ec98b28a87c982fdb4b2e08aa5c3bb23d2863857450194a13bc6ff565cf1426c7e47de2e52cec63b31"

    let session =
        "53616c7465645f5f10b226a63ffd38a2e4e417f9686924ec98b28a87c982fdb4b2e08aa5c3bb23d2863857450194a13bc6ff565cf1426c7e47de2e52cec63b31"

    let url = sprintf "https://adventofcode.com/2023/day/%d/input" day

    task {
        use client = new System.Net.Http.HttpClient()
        client.DefaultRequestHeaders.Add("Cookie", sprintf "session=%s" session)
        return! url |> client.GetStringAsync
    }
    |> Async.AwaitTask
    |> Async.RunSynchronously


type 'a Weights when 'a: comparison = Map<int, 'a list> * Map<'a, int>

module Weights =
    let empty: 'a Weights = Map.empty, Map.empty

    let add weight point ((pq, map): 'a Weights) : 'a Weights =
        let change =
            function
            | Some points -> List.distinct (point :: points)
            | None -> [ point ]
            >> Some

        (Map.change weight change pq, Map.add point weight map)

    let tryFind (pt: 'a) ((_, map): 'a Weights) : int option = Map.tryFind pt map

    let rec private pop f ((pq, map) as weights) =
        if Map.isEmpty pq then
            None, weights
        else
            match f pq with
            | weight, point :: rest -> (Some(weight, point)), (Map.add weight rest pq, Map.remove point map)
            | weight, [] -> pop f (Map.remove weight pq, map)

    let popMax (weights: 'a Weights) : ((int * 'a) option) * 'a Weights when 'a: comparison =
        pop Map.maxKeyValue weights

    let popMin (weights: 'a Weights) : ((int * 'a) option) * 'a Weights when 'a: comparison =
        pop Map.minKeyValue weights

let dot (graph: string) =
    let procStart =
        System.Diagnostics.ProcessStartInfo(
            RedirectStandardInput = true,
            RedirectStandardOutput = true,
            FileName = "dot",
            Arguments = "-Tsvg"
        )

    let proc = System.Diagnostics.Process.Start procStart
    proc.StandardInput.Write(graph)
    proc.StandardInput.Close()
    proc.StandardOutput.ReadToEnd()
