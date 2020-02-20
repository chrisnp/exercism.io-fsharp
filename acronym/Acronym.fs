module Acronym

open System 

let abbreviate (phrase : string) = 
    phrase.Split([|' '; '-'; '_'|], 
                 StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map  (Char.ToUpper << Seq.head)
    |> Seq.fold (fun acro char -> 
                        acro + (char |> string)) 
                ""
    
