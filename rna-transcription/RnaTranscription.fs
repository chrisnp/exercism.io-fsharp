module RnaTranscription

let ERR = "Invalid nucleotide"
let toRna (dna: string): string = 
    dna
    |> String.map (fun x -> match x with
                            | 'G' -> 'C'
                            | 'C' -> 'G'
                            | 'T' -> 'A'
                            | 'A' -> 'U'
                            | _ -> failwith ERR)
