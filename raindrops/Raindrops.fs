module Raindrops

let convert (number: int): string =     
    let rainFactors = [(3, "Pling"); (5, "Plang"); (7, "Plong")]
    let drops = [for (n, s) in rainFactors do if number % n = 0 then yield s]  
    match drops with 
    | []   -> string number
    | drop -> drop |> String.concat ""