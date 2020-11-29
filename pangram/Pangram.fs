module Pangram

let isPangram (input: string): bool = 
    Set.isSuperset 
        (input.ToLower() 
         |> Set.ofSeq) (['a' .. 'z'] 
         |> Set.ofList)
    
