module DiffieHellman

open System

/// Auxiliary
let modexp _base_ exponent modulus =
    let rec loop b e m =
        if e = 0I then 
            m 
        else 
            loop (b * b % modulus) (e >>> 1) 
                 (if e &&& 1I = 0I then m else m * b % modulus)
    loop _base_ exponent 1I

/// API
let privateKey (primeP: bigint): bigint = 
    Random().Next(2, int primeP - 1) 
    |> Numerics.BigInteger

let publicKey primeP primeG privateKey = 
    modexp primeG privateKey primeP

let secret primeP publicKey privateKey = 
    modexp publicKey privateKey primeP