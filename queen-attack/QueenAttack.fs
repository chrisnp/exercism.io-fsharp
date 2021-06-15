module QueenAttack

let create (position: int * int) = 
    let (x, y) = position
    let onChessBoard z = z >= 0 && z < 8
    onChessBoard x && onChessBoard y

let canAttack (queen1: int * int) (queen2: int * int) = 
    let (x1, y1) = queen1
    let (x2, y2) = queen2
    match (x1 - x2 |> abs, y1 - y2 |> abs) 
        with
        | (0, 0) -> failwith "Error: Both queens on same square"
        | (0, _) -> true  // horizontal attack
        | (_, 0) -> true  // vertical attack
        | (x, y) -> x = y // diagonal attack, if true
