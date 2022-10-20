module AtbashCipher
open System

let private atbash c =
    let z = 'z' |> int
    let a = 'a' |> int
    match Char.IsLetter c with
    | true -> (z - (Char.ToLower >> int <| c) + a) |> char 
    | _    -> c

let private chunksOf n = 
    n |> Seq.chunkBySize >> Seq.map String >> String.concat " "

let decode = String.filter Char.IsLetterOrDigit >> String.map atbash

let encode = decode >> chunksOf 5
