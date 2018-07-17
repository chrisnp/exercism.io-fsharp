module RnaTranscription

open System
let compl (nuc : Char) : Char =
    [('G', 'C');('C', 'G');('T', 'A');('A', 'U')]
    |> Map.ofSeq
    |> Map.find nuc

let toRna (dna: string): string = 
    dna
    |> Seq.map compl
    |> Seq.toArray
    |> System.String


