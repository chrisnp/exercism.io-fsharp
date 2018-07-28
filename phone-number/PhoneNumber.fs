module PhoneNumber

open System 
let clean (input : string) : string option  = 
    let valid ch = ch <> '0' && ch <> '1'
    let digits = input |> List.ofSeq |> List.filter Char.IsDigit
    match (digits.Length, digits) with
    | 11, '1':: num 
    | 10, num when (valid num.[0]) && (valid num.[3]) ->
        num
        |> (Array.ofList >> String) 
        |> Some
    | _ -> None  