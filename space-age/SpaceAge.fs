module SpaceAge

open System

type Planet = 
    | Earth
    | Venus
    | Mars
    | Jupiter
    | Saturn
    | Uranus
    | Neptune
    | Mercury

let orbitalPeriodOf = 
    function
    | Mercury -> 0.2408467
    | Venus   -> 0.61519726
    | Earth   -> 1.0
    | Mars    -> 1.8808158 
    | Jupiter -> 11.862615
    | Saturn  -> 29.447498
    | Uranus  -> 84.016846
    | Neptune -> 164.79132
    
let age (planet: Planet) (seconds: int64): float = 
    let round (n : float) = Math.Round (n, 2)
    let earthYear = 31557600.0
    float seconds / (earthYear  * orbitalPeriodOf planet)
    |> round