module HighScores

let scores (values: int list): int list = values

let latest (values: int list): int = List.last values

let highest (values: int list): int = List.max values

let top  (values: int list): int list = (List.sortDescending >> List.truncate 3) values

let report (values: int list): string = 
    let diff = highest values - latest values

    (latest values, if diff = 0 then " " else sprintf " %i short of " diff)
    ||> 
    sprintf "Your latest score was %i. That's%syour personal best!"