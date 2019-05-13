module SimpleCipher

open System

let private inValid key = key = "" || not <| Seq.forall (fun c -> Char.IsLower(c)) key 
let private xmod(x: int, y: int) = (y % x + x) % x
let private charToIndex c = int c - int 'a'
let private indexToChar i = i + int 'a' |> char

let private xShift (input:string) (key:string) op = 
    Seq.zip key.[..(input |> Seq.length) - 1] input
    |> Seq.map (fun (k, c) -> op (charToIndex k) (charToIndex c) >> xmod >> indexToChar)
    |> Seq.toArray
    |> String

type SimpleCipher(key: string) =
    
    member __.Key with get() = key
    
    member __.Encode(plaintext) = xShift plaintext key (+) 
    
    member __.Decode(ciphertext) = xShift ciphertext key (-)
    
    new() = Array.init 100 (fun _ -> List.item (Random().Next( ['a'..'z'] |> List.length)) ['a'..'z']) 
            |> String