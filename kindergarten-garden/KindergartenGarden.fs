module KindergartenGarden

type Plant = | Grass 
             | Radishes 
             | Clover 
             | Violets 
             | Unknown

type Garden = { Row0 : Plant list; Row1 : Plant list }

let private students: string list = 
        (Seq.sort >> Seq.toList) 
        <| ["Alice";"Ginny";"Charlie";
            "David";"Joseph";"Fred"; 
            "Bob";"Harriet";"Ileana";
            "Eve";"Kincaid";"Larry"]

let private decodePlant (code: char): Plant =
        match code with
        | 'G' -> Plant.Grass
        | 'R' -> Plant.Radishes
        | 'C' -> Plant.Clover
        | 'V' -> Plant.Violets
        |  _  -> Plant.Unknown

let private garden (diagram: string): Garden = 
        let rows  = 
                diagram.Split('\n')
                |> Seq.map (Seq.map decodePlant >> Seq.toList)
                |> Seq.toList
        { Row0 = rows.[0]; Row1 = rows.[1] }      

let plants diagram student = 
        let i = students |> List.findIndex ((=) student) |> (*) 2
        [ 
          (garden diagram).Row0.[i]; (garden diagram).Row0.[i + 1]
          (garden diagram).Row1.[i]; (garden diagram).Row1.[i + 1] 
        ]