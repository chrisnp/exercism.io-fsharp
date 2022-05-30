module TisburyTreasureHunt

let getCoordinate (line: string * string): string =
    snd line

let convertCoordinate (coordinate: string): int * char = 
    coordinate.[0..0] |> int, coordinate.[1]

let compareRecords (azarasData: string * string) (ruisData: string * (int * char) * string) : bool = 
    let azCoord =  azarasData |> getCoordinate |> convertCoordinate
    match ruisData with
    | _, ruCoord, _ when ruCoord = azCoord -> true
    | _,       _, _                        -> false

let createRecord (azarasData: string * string) (ruisData: string * (int * char) * string) : (string * string * string * string) =
    let treasure, coordinate  = azarasData
    let location, _, quadrant = ruisData
    match compareRecords azarasData ruisData with
    | true -> coordinate, location, quadrant, treasure
    | _    -> "", "", "", ""
