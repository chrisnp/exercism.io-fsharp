module RotationalCipher

open System

let rot26 (key, ch) =
    let A = int 'A'
    let a = int 'a'
    match ch with   
    | ch when (ch |> Char.IsUpper) -> A + (int ch + key - A) % 26 |> char
    | ch when (ch |> Char.IsLower) -> a + (int ch + key - a) % 26 |> char
    | ch when (ch |> Char.IsLetter |> not) -> ch
    | _ -> failwith "Error"

let rotate shiftKey text = 
    text
    |> Seq.map (fun x -> (shiftKey, x) |> rot26)
    |> String.Concat