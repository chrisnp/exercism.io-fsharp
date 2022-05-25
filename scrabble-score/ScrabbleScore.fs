module ScrabbleScore

open System

let private letterValue = function
    | 'Q' | 'Z'                   -> 10
    | 'J' | 'X'                   -> 8
    | 'K'                         -> 5
    | 'F' | 'H' | 'V' | 'W' | 'Y' -> 4
    | 'B' | 'C' | 'M' | 'P'       -> 3
    | 'D' | 'G'                   -> 2
    | 'A' | 'E' | 'I' | 'O' | 'U'
    | 'L' | 'N' | 'R' | 'S' | 'T' -> 1
    | _                           -> 0

let score (word: string) = word 
    |> Seq.sumBy (Char.ToUpperInvariant 
                  >> letterValue)