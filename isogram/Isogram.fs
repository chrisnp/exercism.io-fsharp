module Isogram
open System

let isIsogram (str : string) =
    let normalized = 
        str.ToLowerInvariant() 
        |> Seq.filter Char.IsLetterOrDigit  
    normalized |> Set.ofSeq |> Set.count
    |> (=) (normalized |> Seq.length )      