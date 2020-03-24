module Hamming

let distance (s1: string) (s2: string): int option = 
    if (s1.Length = s2.Length) then
        Seq.zip s1 s2
        |> Seq.filter (fun (x, y) -> x <> y)
        |> Seq.length
        |> Some
    else 
        None
        