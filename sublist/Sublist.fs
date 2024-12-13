module Sublist

type SublistType = Equal | Sublist | Superlist | Unequal

let private isSublist (xs: 'a list) (ys: 'a list): bool =
    ys = [] || 
    xs |> List.windowed (max 1 (min xs.Length ys.Length))
       |> List.contains ys

let sublist (xs: 'a list) (ys: 'a list): SublistType = 
    match (isSublist ys xs, isSublist xs ys) with
    | (true, true)  -> Equal
    | (true, false) -> Sublist
    | (false, true) -> Superlist
    | (_, _)        -> Unequal