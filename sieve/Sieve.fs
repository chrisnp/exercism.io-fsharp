module Sieve

open System.Collections.Generic

let rec sieve limit = 
    seq { yield 2
          let composites = 
              new HashSet<int>()
          for i in 3..2..limit do
              let found = 
                  composites.Contains i
              if not found then
                  yield i
              do for j in i..i..limit do
                  composites.Add j 
                  |> ignore }
                 
let primes limit = 
        match limit with
            | 0 | 1 -> []
            | _     -> sieve limit |> Seq.toList
