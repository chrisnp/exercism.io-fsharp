module Etl

let transform (legacyData: Map<int, char list>): Map<char, int> = 
    let flipPairs (score : int, letters : char list) = 
        letters 
        |> Seq.map (fun letr -> 
                        (char ((string letr).ToLowerInvariant()), 
                         score))
    legacyData
    |> Map.toSeq
    |> Seq.collect flipPairs
    |> Map.ofSeq