module NthPrime

open System.Collections.Generic

// Reusing Sieve.fs code
let rec sieve limit = 
    seq { 
        yield 2
        let composites = new HashSet<int>()
        for i in 3..2..limit do
            let found = composites.Contains i
            if not found then
                yield i
            do for j in i..i..limit do 
                composites.Add j |> ignore 
    }

let primes limit = 
        match limit with
            | 0 | 1 -> []
            | _     -> sieve limit |> Seq.toList

let prime nth : int option = 
    let theoreticalLimit p : int =
        let n = float p 
        (2. + (((4. / 3.) * n * System.Math.Log2(n |> float)))) 
        |> int
    if nth <= 0 then 
        None
    else 
        primes (theoreticalLimit <| nth) |> Seq.item (nth - 1) 
        |> Some