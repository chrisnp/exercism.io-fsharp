module NucleotideCount

// let private valid n = List.contains n ['A';'C';'G';'T']
let privare valid (nuc : char) : char =
  match Seq.contains nuc ['A';'C';'G';'T'] with
  | true -> nuc
  | _ -> failwithf "Invalid nucleotide: %c" nuc

let private initMap = 
    Seq.init 4 (fun _ -> 0) |> Seq.zip ['A';'C';'G';'T'] |> Map.ofSeq

let update f (map:Map<char,int>) k = Map.add (valid k) (f map.[k]) map
let nucleotideCounts (strand: string): Option<Map<char, int>> =  
    match Seq.forall (fun nuc -> valid nuc) strand with
    | true -> (Seq.fold (update ((+) 1)) init strand) >> Some
    | false -> None

    List.map (fun nuc -> (nuc, count nuc strand)) ['A';'C';'G';'T'] 
    |> Map.ofSeq 
    |> Some 
    
let count (nuc : char) (strand : string) =
    match valid nuc with
    | true -> Seq.fold (fun acc x -> if x = nuc then acc + 1 else acc) 0 strand
    | false -> failwithf "invalid nucleotide : %c" nuc


// let nucleotides = "ACTG"
// let init =
//   Seq.initInfinite (fun _ -> 0)
//   |> Seq.zip nucleotides
//   |> Map.ofSeq

// let valid c =
//   match Seq.contains c nucleotides with
//   | true -> c
//   | _ -> failwithf "Invalid nucleotide %c" c

// let update f (map:Map<char,int>) k = Map.add (valid k) (f map.[k]) map

// let nucleotideCounts strand = Seq.fold (update ((+) 1)) init strand |> Some

// // let count c strand = (nucleotideCounts strand).[valid c]