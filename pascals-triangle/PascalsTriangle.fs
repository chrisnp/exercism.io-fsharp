module PascalsTriangle

let rows numberOfRows : int list list = 
    let binomial x y = List.fold (fun r i -> r * (x - i + 1) / i ) 1 [1..y]
    // if numberOfRows < 0 then 
    //     failwith "Nr of rows cannot be less than zero"
    seq {for x in 0..(numberOfRows - 1) do
            yield seq {for y in 0..x do
                            yield binomial x y
                      } 
            |> List.ofSeq
        } 
        |> List.ofSeq       
