module Aoc

let fetchInput day =
  let session = "53616c7465645f5f10b226a63ffd38a2e4e417f9686924ec98b28a87c982fdb4b2e08aa5c3bb23d2863857450194a13bc6ff565cf1426c7e47de2e52cec63b31"
  let url = sprintf "https://adventofcode.com/2023/day/%d/input" day
  task {
    use client = new System.Net.Http.HttpClient()
    client.DefaultRequestHeaders.Add("Cookie", sprintf "session=%s" session)
    return! url |> client.GetStringAsync
  } |> Async.AwaitTask |> Async.RunSynchronously
