module Sieve

let rec sieve primes = function
    | head::tail -> sieve (head::primes) (tail |> List.filter (fun curr -> curr % head > 0)) 
    | [] -> primes |> List.rev

let primes limit = sieve [] [2..limit]