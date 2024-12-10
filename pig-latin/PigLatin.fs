module PigLatin

let private vowel (c: char): bool = "aeiou" |> Seq.contains c

let private piglatinize (input: string): string = 
    match input |> Seq.toList with
    | 'c'::'h'::_               -> $"{input.[2..]}ch"
    | 's'::'c'::'h'::_          -> $"{input.[3..]}sch"
    | 'r'::'h'::'y'::_          -> $"{input.[2..]}rh"    
    | 't'::'h'::'r'::_          -> $"{input.[3..]}thr"
    | 't'::'h'::_               -> $"{input.[2..]}th"
    | 'q'::x::_ when x <> 'u'   -> $"{input.[1..]}q"
    | 'q'::_                    -> $"{input.[2..]}qu"
    | 'x'::'r'::_               -> $"{input}"
    | 'y'::'t'::_               -> $"{input}"
    | x::_ when vowel x         -> $"{input}"
    | x::'q'::'u'::_ 
        when (not << vowel) x   -> $"{input.[3..]}{x}qu"
    | x::_ 
        when "pkxy".Contains x  -> $"{input.[1..]}{x}"
    | x::['y']                  -> $"{input.[1]}{x}"
    | _                         -> $"{input.[1..]}{input.[0]}"
    |> fun x -> x + "ay"

let translate (input: string) : string =
    input.Split(" ") 
    |> Seq.map piglatinize 
    |> String.concat " "



