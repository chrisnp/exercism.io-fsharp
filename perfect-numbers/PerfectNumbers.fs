﻿module PerfectNumbers

open System 


type Classification = Perfect 
                      | Abundant 
                      | Deficient 

let classify n : Classification option = 
    let aliquotSum = 
        [1..n/2]
        |> Seq.filter ((=) 0 << (%) n) 
        |> Seq.sum
    if n < 1 then 
        None
    else 
        match aliquotSum with
        | _ when aliquotSum < n -> 
            Some Deficient
        | _ when aliquotSum > n -> 
            Some Abundant
        | _ -> 
            Some Perfect