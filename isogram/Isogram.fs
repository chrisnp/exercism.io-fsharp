module Isogram

open System
let isIsogram (str: string) =
    let normal = str.ToLowerInvariant() |> Seq.filter System.Char.IsLetterOrDigit 
    normal
    |> Set.ofSeq |> Set.count = Seq.length normal       