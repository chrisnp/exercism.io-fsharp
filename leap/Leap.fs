module Leap

let leapYear (year: int): bool = 
    let by400 = 
        year % 400
    let by100 = 
        year % 100
    let by004 = 
        year % 4
    match (by400, by100, by004) 
        with
        | (0, 0, 0) -> true
        | (_, 0, 0) -> false
        | (_, _, 0) -> true
        | (_, _, _) -> false
