module WordCount

open System

let countWords (phrase: string) = 
    let allowed (ch: char) =
        Array.contains ch [|' ';'\'';','|]
        || ch |> Char.IsDigit 
        || ch |> Char.IsLetter 
    let sanitized = 
        phrase.ToLowerInvariant()
        |> Seq.filter allowed
        |> String.Concat
    let tokens = 
        sanitized.Split([|' ';','|])
        |> Seq.filter (String.IsNullOrEmpty >> not)
        |> Seq.map (fun x -> x.Trim('''))
    tokens
    |> Seq.groupBy id
    |> Map.ofSeq
    |> Map.map (fun i v -> Seq.length v)