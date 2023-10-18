module Allergies

open System

type Allergen = 
    | Eggs         = 0x01
    | Peanuts      = 0x02
    | Shellfish    = 0x04
    | Strawberries = 0x08
    | Tomatoes     = 0x10
    | Chocolate    = 0x20
    | Pollen       = 0x40
    | Cats         = 0x80

let allergicTo codedAllergies allergen = 
    int allergen &&& codedAllergies <> 0

let list codedAllergies = 
    Enum.GetValues typeof<Allergen>
    |> Seq.cast<Allergen>
    |> Seq.filter (allergicTo codedAllergies)
    |> Seq.toList