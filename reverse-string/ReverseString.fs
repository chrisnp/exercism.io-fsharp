module ReverseString

let reverse (input: string): string = 
    let (><) f a b = f b a
    input 
    |> Seq.fold ((><) (fun x y -> x :: y)) []
    |> Seq.toArray
    |> System.String