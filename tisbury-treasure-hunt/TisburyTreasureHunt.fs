module TisburyTreasureHunt

let getCoordinate (line: string * string): string =
    let (_, coordinate) = line
    coordinate

let convertCoordinate (coordinate: string): int * char = 
    match coordinate |> Seq.toList with
    | [a;b] -> (a |> string |> int, b)
    | _ -> failwith "Invalid coordinate"

let compareRecords (azarasData: string * string) (ruisData: string * (int * char) * string) : bool = 
    let (_, ruisCoordinate, _) = ruisData
    ruisCoordinate = (azarasData |> getCoordinate |> convertCoordinate)

let createRecord (azarasData: string * string) (ruisData: string * (int * char) * string) : (string * string * string * string) =
    if compareRecords azarasData ruisData then
        let (azarasName, coordinate) = azarasData
        let (ruisName, _, color) = ruisData
        (coordinate, ruisName, color, azarasName)
    else
        ("", "", "", "")
