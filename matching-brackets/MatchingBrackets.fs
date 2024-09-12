module MatchingBrackets

let brackets = ['[';'{';'(';')';'}';']']

let isPaired (input: string): bool = 
    let rec paired bs stack =
        match bs, stack with
        // | _ :: _ -> false
        | ('('|'['|'{')::xs, stack -> paired xs (Seq.head xs)::stack
        | ')'::xs, '('::ys -> paired xs ys
        | '}'::xs, '{'::ys -> paired xs ys
        | ']'::xs, '['::ys -> paired xs ys
        | [], [] -> true
        | _, _ -> false
    input 
    |> Seq.toList 
    |> List.filter (fun c -> brackets |> List.contains c)
    |> paired []
