module ProteinTranslation

open System

let translateCodon = function
  | "AUG" -> 
      "Methionine"
  | "UUU" | "UUC" -> 
      "Phenylalanine"
  | "UUA" | "UUG" -> 
      "Leucine"
  | "UCU" | "UCC" | "UCA" | "UCG" -> 
      "Serine"
  | "UAU" | "UAC" -> 
      "Tyrosine"
  | "UGU" | "UGC" -> 
      "Cysteine"
  | "UGG" -> 
      "Tryptophan"
  | "UAA" | "UAG" | "UGA" -> 
      "STOP"
  | other -> 
      failwithf "Not valid codon: %s" other

let proteins rna = 
    let stopped cod = cod = "STOP"
    rna 
    |> Seq.chunkBySize 3 
    |> List.ofSeq 
    |> List.map (String >> translateCodon) 
    |> List.takeWhile (not << stopped) 