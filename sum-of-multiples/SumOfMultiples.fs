module SumOfMultiples

let sum (numbers: int list) (upperBound: int): int = 
    [1 .. upperBound - 1]
    |> Seq.filter (fun possible -> 
                    numbers 
                    |> Seq.exists (fun x -> 
                                   if x = 0 then 
                                    false 
                                   else 
                                    possible % x = 0)
                  )
    |> Seq.sum