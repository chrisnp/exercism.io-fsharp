module PerfectNumbers

type Classification = Perfect | Abundant | Deficient 

let classify n : Classification option = 
    if n <= 0 then None
    else
        let aliquotSum = [1 .. n/2] |> List.filter (fun i -> n % i = 0) |> List.sum 
        match aliquotSum with 
        | _ when aliquotSum < n -> Some Classification.Deficient
        | _ when aliquotSum > n -> Some Classification.Abundant
        | _ -> Some Classification.Perfect