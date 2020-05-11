module ArmstrongNumbers

let isArmstrongNumber (number: int): bool = 
    let digits = 
        [for d in string number -> int (d.ToString())]
    let length = 
        string <| number |> String.length
    digits 
    |> List.sumBy (fun x -> 
                    int (float x ** float length))
    |> (=) number

