module Bob

open System
   
let response (input : string) : string = 
    let shout : int = 
        match (input |> Seq.exists(Char.IsLetter)) && 
                        input = input.ToUpperInvariant() 
            with
            | true  -> 1
            | false -> 0 
    let question : int = 
        match input.Trim().EndsWith("?") 
            with
            | true  -> 1
            | false -> 0
    let silent : int = 
        match input 
              |> String.forall (System.Char.IsWhiteSpace) 
            with
            | true  -> 1
            | false -> 0 
    match (shout, question, silent) with
    | (1, 1, 0) -> "Calm down, I know what I'm doing!"
    | (0, 0, 1) -> "Fine. Be that way!"
    | (0, 1, 0) -> "Sure."
    | (1, 0, 0) -> "Whoa, chill out!" 
    | (_, _, _) -> "Whatever."