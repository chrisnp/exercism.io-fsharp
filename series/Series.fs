module Series

let slices (str : string) (length : int) = 
    if length > str.Length || length <= 0
    then 
        None
    else
        str |> Seq.windowed length
            |> (Seq.map <| System.String)
            |> Seq.toList
            |> Some