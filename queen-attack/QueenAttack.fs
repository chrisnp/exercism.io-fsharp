module QueenAttack

let create (position: int * int) = 
    let (x, y) = position
    let onChessBoard z = z >= 0 && z < 8
    onChessBoard x && onChessBoard y

let canAttack (queen1: int * int) (queen2: int * int) = 
    let (x1, y1) = queen1
    let (x2, y2) = queen2
    let horizontalAttack = x1 = x2
    let verticalAttack   = y1 = y2
    let diagonalAttack   = (x2 - x1 |> abs) = (y2 - y1 |> abs)
    horizontalAttack || verticalAttack || diagonalAttack