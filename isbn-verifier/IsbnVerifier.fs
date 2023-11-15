module IsbnVerifier
open System

let isValid isbn = 
    let digits = 
        isbn
        |> Seq.filter System.Char.IsLetterOrDigit
        |> Seq.map( fun d -> if System.Char.IsDigit(d) 
                             then (int << string) d
                             elif d = 'X' 
                             then 10 
                             else -1)
    let errors = digits |> Seq.contains -1 || digits |> Seq.length <> 10 || 
                 digits |> (Seq.take 9 >> Seq.contains 10) 
    match errors with
        | true -> false
        | _ -> (Seq.foldBack (fun d (csum, idx) -> (csum + d * idx, idx + 1)) 
                             digits (0, 1) |> fst) % 11 |> (=) 0  
     