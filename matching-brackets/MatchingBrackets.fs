module MatchingBrackets

let isPaired (input: string): bool = 
    let rec paired (brackets: char list) (stack: char list): bool =
        match (brackets, stack) with
        | ([], ys)      -> ys |> List.isEmpty
        | ('('::xs, ys) -> paired (xs) ('('::ys)
        | ('{'::xs, ys) -> paired (xs) ('{'::ys)
        | ('['::xs, ys) -> paired (xs) ('['::ys)
        | (')'::xs, '('::ys) 
        | ('}'::xs, '{'::ys) 
        | (']'::xs, '['::ys) -> paired xs ys
        | ((')'|'}'|']')::xs, []) 
        | ((')'|'}'|']')::xs, _)  -> false
        | _::xs, ys -> paired xs ys
    paired (input |> Seq.toList) [] 
