module Bob

open System
   
let response (input : string) : string = 
    let shout : bool = (input |> Seq.exists(Char.IsLetter)) && input = input.ToUpperInvariant() 
    let question : bool = input.Trim().EndsWith("?")
    let silent : bool = input |> String.forall (System.Char.IsWhiteSpace) 
    
    match input with
    | _ when silent             -> "Fine. Be that way!"
    | _ when question && shout  -> "Calm down, I know what I'm doing!"
    | _ when shout              -> "Whoa, chill out!"  
    | _ when question           -> "Sure."
    | _                         -> "Whatever."