module Anagram

open System

let findAnagrams (sources: string list) (target: string) = 
    
    let normal (x: string) = 
        x.ToLower() 
        |> Seq.sortBy id 
        |> Seq.toList

    List.filter (fun x -> normal x = normal target) sources
    |> List.filter (fun x -> x.ToLower() <> target.ToLower())