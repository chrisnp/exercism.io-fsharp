module AllYourBase
open FsUnit

let inline (><) (x, y) = y, x 

let rebase digits inputBase outputBase = 
    let errors =
        [inputBase;outputBase] |> List.exists ((<=) 2)
        || digits |> List.exists ((<) 0)
        || digits |> List.exists ((>=) inputBase)

    let degenerate =
        digits |> List.isEmpty 
        || digits |> List.forall((=) 0)
       

    let representationIn newBase num =
        if num > 0 then 
            num
            |> List.unfold (fun (x: int) -> 
                    if x = 0 then 
                        None 
                    else 
                        Some ((><) <| System.Math.DivRem(x, newBase)))  
            |> List.rev
        else
            List.empty

    if   errors then 
        None
    elif degenerate then 
        Some[0]
    else
        digits
        |> Seq.zip [(List.length digits - 1) .. -1 ..0]
        |> Seq.sumBy (fun (i, d) -> d * (int) ((float) inputBase ** (float) i))
        |> representationIn outputBase
        |> Some
