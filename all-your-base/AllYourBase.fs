module AllYourBase

let inline (><) (x, y) = y, x 

let rebase digits inputBase outputBase = 
    let errors =
        [inputBase;outputBase] |> List.exists (fun x -> x <= 1)
        || digits |> List.exists (fun x -> x < 0)
        || digits |> List.exists (fun x -> x >= inputBase)

    let degenerate =
        digits |> List.forall(fun x -> x = 0)

    let numberIn newBase fromDigits = 
        let len = (fromDigits : int list).Length
        fromDigits 
        |> List.mapi (fun p d -> d * int(double(newBase) ** double(len - 1 - p)))
        |> List.sum

    let digitsIn newBase fromNumber = 
        fromNumber
        |> List.unfold (fun x -> 
                            if x = 0 then 
                                None 
                            else 
                                Some((><) <| System.Math.DivRem(x, newBase)))
        |> List.rev

    if   errors then 
        None
    elif degenerate then 
        Some[0]
    else
        Some (digits |> numberIn inputBase |> digitsIn outputBase)