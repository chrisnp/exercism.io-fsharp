module RunLengthEncoding

open System

let private (|Count|_|) (str: String) =
    if str |> String.length = 0 then 
        None
    else 
        let len = str |> Seq.takeWhile((=) str.[0]) 
                      |> Seq.length
        Some (str.Substring(0, 1), len, str.[len..]) 

let rec encode input = 
    let emit times letter =
        match times with
        | 1 -> letter
        | x -> String.Format ("{0}{1}", x, letter)
    match input with
    | "" -> ""
    | Count (n, c, rest) -> (emit c n) + (encode rest) 
    | _ -> failwith "Here to prevent warning FS0025 (tail recursion is preserved, with previous pattern)"

let private (|Digits|_|) (str: String)= 
    let dsMatch = 
        Text.RegularExpressions.Regex.Match(str, "^([0-9]+)?([\w\s])")
    match dsMatch.Success, dsMatch.Groups.[1].Value with
    | true, "" -> 
        Some (1, char dsMatch.Groups.[2].Value, str[dsMatch.Length..])
    | true, n ->
        Some (int n, char dsMatch.Groups.[2].Value, str[dsMatch.Length..])
    | false, _ -> None

let rec decode input = 
    match input with
    | "" -> ""
    | Digits (n, c, rest) -> String(c, n) + (decode rest)
    | _ -> failwith "Here to prevent warning FS0025 (tail recursion is preserved though..)"