module Darts

let score (x: double) (y: double): int = 
    match sqrt <| (+) (x**2.) (y**2.)
        with 
        | r when r <= 1.  -> 10
        | r when r <= 5.  -> 5
        | r when r <= 10. -> 1
        | _ -> 0