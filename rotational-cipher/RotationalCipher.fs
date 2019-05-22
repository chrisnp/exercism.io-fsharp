module RotationalCipher

open System

let rot26 key ch =
    match ch with   
    | ch when (ch |> Char.IsUpper) -> (int ch + key - int 'A') % 26 + (int 'A') |> char
    | ch when (ch |> Char.IsLower) -> (int ch + key - int 'a') % 26 + (int 'a') |> char
    | ch when (ch |> Char.IsLetter |> not) -> ch
    | _ -> failwith "Error"

let rotate shiftKey text = 
    text
    |> Seq.map (fun x -> rot26 shiftKey x)
    |> String.Concat