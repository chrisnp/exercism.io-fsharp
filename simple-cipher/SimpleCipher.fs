module SimpleCipher

open System

let private charToIndex c = int c - int 'a'
let private indexToChar i = i + int 'a' |> char
let private mod26 (x: int) = if x < 0 then x + 26 else x % 26

let private xShift (input:string) (key:string) (sign:int) = 
    input 
    |> Seq.zip ( seq{ while true do for i in key -> i } )
    |> Seq.map (fun (k, c) -> ((c |> charToIndex) + sign * (k |> charToIndex)) |> mod26 |> indexToChar)
    |> String.Concat

type SimpleCipher(key: string) =
    
    member __.Key with get() = key
    
    member __.Encode(plaintext)  = xShift plaintext key 1 
    
    member __.Decode(ciphertext) = xShift ciphertext key (-1)
    
    new() = SimpleCipher ([for i in 1 .. 100 do yield char (int 'a' + Random().Next(26))]
                          |> String.Concat)