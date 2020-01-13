module ArmstrongNumbers

let isArmstrongNumber (number: int): bool = 
    let digits = [for ch in string number -> int (ch.ToString())]
    let length = string <| number |> String.length
    digits 
    |> List.sumBy (fun x -> int (float x ** float length))
    |> (=) number

