module Proverb

let recite (input: string list): string list = 
    match input with
    | []  -> []
    | [x] -> [ sprintf "And all for the want of a %s." x ]
    | _   -> seq { 
                yield! 
                    input 
                    |> List.pairwise 
                    |> List.map(fun (x, y) -> 
                                sprintf "For want of a %s the %s was lost." 
                                <|| (x, y))
                yield  
                    sprintf "And all for the want of a %s." input.Head 
             } 
             |> Seq.toList