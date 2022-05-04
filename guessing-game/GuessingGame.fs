module GuessingGame

let reply (guess: int): string = 
    let target = 42
    match (guess - target) with
    | 0 -> "Correct"
    | x when abs x <= 1 -> "So close"
    | x when x > 1 -> "Too high"
    | _ -> "Too low"
