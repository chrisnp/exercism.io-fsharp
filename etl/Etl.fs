module Etl

let transform (scoresWithLetters: Map<int, char list>): Map<char, int> = 
    let flipPairs (score : int, letters : char list) = 
        letters 
        |> Seq.map (fun letter -> 
                    (char ((string letter).ToLowerInvariant()), score))
    scoresWithLetters
    |> Map.toSeq
    |> Seq.collect flipPairs
    |> Map.ofSeq