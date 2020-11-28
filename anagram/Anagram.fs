module Anagram

open System

let findAnagrams (sources: string list) (target: string) = 
    let normal (x: string) = 
        x.ToLower() 
        |> Seq.sortBy id 
        |> Seq.toList
    sources
    |> List.filter (fun x -> 
                 normal x = normal target) 
    |> List.filter (fun x -> 
                    x.ToLower() <> target.ToLower())