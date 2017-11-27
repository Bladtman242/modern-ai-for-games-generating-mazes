module SeqExtensions
module Seq =
    let infinite (e : 'a) : 'a seq = Seq.initInfinite (fun _ -> e)
