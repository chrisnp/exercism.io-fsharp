module Grains

let square (n: int): Result<uint64,string> = 
    match n with 
    | _ when n <= 0 || n > 64 -> Error "Invalid input"
    | _ ->  ((pown 2.0 (n - 1)) |> uint64) |> Ok

let total: Result<uint64,string> = 
    [| 1..64 |] 
    |> Seq.sumBy (fun x -> pown 2.0 (x - 1) |> uint64)  
    |> Ok
 