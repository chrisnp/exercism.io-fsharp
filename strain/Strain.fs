module Seq

let rec keep pred xs = 
    [for x in xs do 
        if x |> pred then yield x ]

let discard pred xs = 
    keep (not << pred) xs