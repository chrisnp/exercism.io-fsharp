module Acronym

open System 

let abbreviate (phrase : string) = 
    phrase.Split([|' '; '-'|], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map (Char.ToUpper << Seq.head)
    |> Seq.fold (fun acro ch -> acro + (ch |> string)) ""
    
