module PascalsTriangle

let rows numberOfRows : int list list = 
    let rec binomialc n k = 
        if k = n || k = 0 then 1 
        else (binomialc (n - 1) (k - 1)) + (binomialc (n - 1) k)
    seq { for x in 0..(numberOfRows - 1) do
            yield seq { for y in 0..x do
                            yield binomialc x y
                  } 
                  |> List.ofSeq
    }
    |> List.ofSeq       
