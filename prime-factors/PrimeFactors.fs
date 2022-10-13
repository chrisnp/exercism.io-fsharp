module PrimeFactors

let factors number = 
    let rec sieve num f fs =
        if num = 1L then fs
        elif num < f * f then num :: fs
        elif num % f = 0L then sieve (num / f) f (f :: fs)
        else sieve num (f + 1L) fs
    sieve number 2L [] |> List.map (fun n -> n |> int)
                       |> List.rev
