module CollatzConjecture


let rec converge (length : int, x : int) =
    match x with
        | _ when x < 1 -> 
            None
        | _ when x = 1 -> 
            Some length
        | _ when x % 2 = 0 -> 
            converge (length + 1, (x / 2))
        | _ -> 
            converge (length + 1, (3 * x + 1 ))

let steps (number: int): int option = 
    converge (0, number)


