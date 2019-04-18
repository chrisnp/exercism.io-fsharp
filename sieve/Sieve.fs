module Sieve

open System
open System.Collections

let sieve limit = 
    let primeSieve = Array.create limit true
    let maxp = sqrt (float (limit)) |> int
    primeSieve.[0] <- false
    primeSieve.[1] <- false
    primeSieve.[2] <- true
    for p in 2..(maxp + 1) do
        if primeSieve.[p] then
            for pm in (p * 2)..p..(limit) do
                primeSieve.[pm] <- false
    primeSieve

let primes limit = match limit with
                   | 0 | 1 -> []
                   | _ -> sieve limit
                          |> Seq.mapi (fun i v -> v, i)
                          |> Seq.filter fst
                          |> Seq.map snd
                          |> Seq.toList

