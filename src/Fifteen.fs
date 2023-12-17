module Fifteen

let hash =
  let fold current ascii =
    (current + ascii) * 17 % 256

  Seq.map int >> Seq.fold fold 0


let one: string seq -> int =
  Seq.head >> String.split ',' >> Seq.map hash >> Seq.sum
