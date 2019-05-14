module ArmstrongNumbers

let isArmstrongNumber (number: int): bool = 

    let numberOfDigits = string <| number |> String.length

    [for char in string <| number -> int <| (char |> string)]
    |> List.sumBy (fun x -> int <| float x ** float numberOfDigits)
    |> (=) number

