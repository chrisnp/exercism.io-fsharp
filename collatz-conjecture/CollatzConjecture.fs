module CollatzConjecture

let rec collatzConverge (length: int, x: int) = 
    match x with
    | _ when x < 1 -> None
    | _ when x = 1 -> Some length
    | _ when x % 2 = 0 -> collatzConverge (length + 1, (x / 2))
    | _ -> collatzConverge (length + 1, (3 * x + 1 ))

let steps (number: int): int option = 
    collatzConverge (0, number)


