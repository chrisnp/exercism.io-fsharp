module NucleotideCount

let nucleotides = ['A';'C';'G';'T']

let nucleotideCounts (strand: string): Option<Map<char, int>> =  
    match strand.ToCharArray() |> Array.forall (fun x -> Seq.contains x nucleotides) with
    | true -> nucleotides
              |> Seq.map (fun x -> x, strand |> Seq.filter ((=) x) |> Seq.length)
              |> Map.ofSeq
              |> Some
    | false -> None

    
