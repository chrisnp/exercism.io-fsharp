module CarsAssemble

let private carsPerHour = 221.

let successRate (speed: int): float =
        match speed with
        | 0 -> 0.0
        | 1 | 2 | 3 | 4 -> 1.0
        | 5 | 6 | 7 | 8 -> 0.9
        | 9  -> 0.8
        | 10 -> 0.77
        | _ -> failwith "Invalid Speed"

let productionRatePerHour (speed: int): float =
    speed
    |> successRate
    |> (*) (speed |> float)
    |> (*) carsPerHour

let workingItemsPerMinute (speed: int): int =
    productionRatePerHour speed / 60.0 |> int
