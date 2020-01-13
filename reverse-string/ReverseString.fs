module ReverseString

let reverse (input: string): string = 
    let flip f a b = f b a
    input 
    |> Seq.fold (flip (fun x y -> x :: y)) []
    |> Seq.toArray
    |> System.String