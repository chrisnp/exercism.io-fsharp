module CollatzConjecture

let rec collatzConverge(x: int, length: int) = 
    match x with
    | _ when x < 1 -> None
    | _ when x = 1 -> Some length
    | _ when x % 2 = 0 -> collatzConverge (x / 2, length + 1)
    | _ -> collatzConverge (3 * x + 1, length + 1 )

let steps (number: int): int option = 
    collatzConverge (number, 0)


