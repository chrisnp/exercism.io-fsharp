module PerfectNumbers

open System 


type Classification = Perfect 
                      | Abundant 
                      | Deficient 

let classify n : Classification option = 
    let aliquotSum = 
        [1..n/2]
        |> Seq.filter ((%) n >> (=) 0)
        |> Seq.sum
    if n < 1 then 
        None
    else 
        match aliquotSum with
        | _ when aliquotSum = n -> 
            Some Perfect
        | _ when aliquotSum > n -> 
            Some Abundant
        | _ -> 
            Some Deficient