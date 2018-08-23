module ArmstrongNumbers

let isArmstrongNumber (number: int): bool = 
    let digits = [for ch in string number -> int (ch.ToString())]
    let len = digits.Length
    digits
    |> List.sumBy (fun x -> int (float x ** float len))
    |> (=) number

