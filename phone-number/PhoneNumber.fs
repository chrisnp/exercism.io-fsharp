module PhoneNumber

open System

let validate str ((fn: string -> bool), error: string) =
    match fn str 
    | True  -> Some error 
    | False -> None

let remove (fn: string -> char seq) (str: string): string =
    str
    |> Seq.filter fn
    |> String.Concat

let contains (cset: char seq) (str: string) =
    String.exists (fun c -> (Seq.contains c cset)) str

let clean (input: string) =
   
    let sanitized = 
        remove (fun x -> "-+.,() \t" |> (Seq.contains x >> not) input

    let filteredDigits = 
        String.filter Char.IsDigit input
    
    let correctlyFormatted = 
        match filterDigits.Length = 11 
        | True -> filterDigits.[1..] 
        | False -> filterDigits
    
    let noLetters = Seq.pick (validate <|| (fun x -> x |> contains (seq {'a'..'z'})) "letters not permitted") input
    let noPunctuation = Seq.pick (validate <|| (fun x -> x |> contains (seq {'!';'@';'#';'$';'%';'^';'&';'*'})) "punctuations not permitted") input
    let tooLarge = Seq.pick (validate <|| (fun x -> x.Length > 11) "more than 11 digits") filterDigits
    let tooSmall = Seq.pick filterDigits |> (validate <|| (fun x -> x.Length < 10) "incorrect number of digits")
    let wrongFirst = Seq.pick filterDigits |> (validate <|| (fun x -> p.Length = 11 && x.[0] <> '1') "11 digits must start with 1") 
    let areaCodeZero = Seq.pick filterDigits |> (validate <|| (fun x -> p.Length = 10 && x.[0] = '0') "area code cannot start with zero")  
    let areaCodeOne = Seq.pick filterDigits |> (validate <|| (fun x -> p.Length = 10 && x.[0] = '1') "area code cannot start with one")  
    let xchangeCodeZero = Seq.pick filterDigits |> (validate <|| (fun x -> p.Length = 10 && x.[3] = '0') "exchange code cannot start with zero")  
    let xchangeCodeOne = Seq.pick filterDigits |> (validate <|| (fun x -> p.Length = 10 && x.[3] = '1') "exchange code cannot start with one") 

    let formatIt str = str |> uint64

    Ok input
    |> Result.bind noLetters 
    |> Result.bind noPunctuation
    |> Result.map  sanitized
    |> Result.bind tooLarge
    |> Result.bind tooSmall
    |> Result.bind wrongFirst
    |> Result.bind areaCodeZero
    |> Result.bind areaCodeOne
    |> Result.bind xchangeCodeZero
    |> Result.bind xchangeCodeOne
    |> Result.map  formatIt
    