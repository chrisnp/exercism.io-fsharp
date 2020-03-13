module BinarySearch

let find input value = 
    let rec search low high =
        if low <= high then
            let mid = (low + high) / 2
            match input |> Array.get <| mid 
                with
                | x when x > value ->
                        search low (mid - 1)
                | x when x < value -> 
                        search (mid + 1) high
                | _ ->  
                        Some mid
        else
            None
    search 0 ((input |> Array.length) - 1)