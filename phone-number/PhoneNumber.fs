module PhoneNumber

open System
 
let rec clean (input: string) : Result<uint64, string> =
   if  input |> Seq.exists (Char.IsLetter) then 
      Error "letters not permitted"
   elif input |> Seq.exists (fun c ->  Char.IsPunctuation(c) && 
                                       (['('; ')'; '-'; '.'] 
                                       |> List.contains c |> not)) then 
      Error  "punctuations not permitted"
   else 
      let digits: char list = input |> Seq.filter (Char.IsDigit) |> Seq.toList
      if (digits |> Seq.length) > 11 then 
         Error "more than 11 digits"
      elif (digits |> Seq.length) < 10 then
         Error "incorrect number of digits"
      elif (digits |> Seq.length = 11) && (digits.[0] <> '1') then 
         Error "11 digits must start with 1"
      elif digits |> Seq.length = 11 then
         clean (digits |> Seq.tail |> Seq.toArray |> System.String)
      else 
         if digits.[0] = '0' then 
            Error "area code cannot start with zero"
         elif digits.[0] = '1' then 
            Error "area code cannot start with one"
         elif digits.[3] = '0' then 
            Error "exchange code cannot start with zero"
         elif digits.[3] = '1' then 
            Error "exchange code cannot start with one"
         else
            match System.UInt64.TryParse (digits |> Seq.toArray
                                                 |> System.String) with
            | true, number -> Ok number
            | _, _ -> Error ""
