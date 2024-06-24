module SqueakyClean

open System

let private IsNotGreek (x: char): bool = x < 'α' || 'ω' < x

let transform (c: char) : string =
    if IsNotGreek(c) then
        if c.Equals('-') then "_"
        else if Char.IsWhiteSpace(c) then ""
        else if Char.IsUpper(c) then "-" + c.ToString().ToLower()
        else if Char.IsDigit(c) then ""
        else c.ToString()
    else "?"
    
let clean (identifier: string): string =
    identifier 
    |> String.collect transform
