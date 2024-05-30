module Luhn

open System

let private (<|>) f x y = f y x 

let rec private DoubleEvens acc = 
    let f x = let y = 2 * x in if x < 5 then y else y - 9
    function
    | []           -> acc
    | [x]          -> x :: acc
    | x :: y :: xs -> DoubleEvens (f y :: x :: acc) xs


let valid number = 
    let sanitary = number |> String.filter (Char.IsWhiteSpace >> not)
    match sanitary.Length <= 1 || 
          sanitary |> String.forall Char.IsDigit |> not
    with
    | true -> false
    | _ -> sanitary |> Seq.map (Char.GetNumericValue >> int)
           |> Seq.rev |> List.ofSeq |> DoubleEvens [] |> List.sum
           |> (<|>) (%) 10 |> (=) 0
