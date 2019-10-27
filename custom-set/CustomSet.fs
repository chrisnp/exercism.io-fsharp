module CustomSet


type CustomSet<'a> = { elements : 'a list }

let empty = { elements = [] }

let singleton value = { elements = [value] }

let isEmpty set = set = empty

let size set = set.elements |> List.length

let fromList list = { elements = list |> List.sort |> List.distinct }

let toList set = set.elements

let contains value set = set.elements |> (List.contains <| value)

let insert value set = fromList <| value::set.elements

let union left right = fromList <| left.elements @ right.elements

let intersection left right = left.elements
                              |> List.filter (fun x -> right.elements 
                                                       |> (List.contains <| x))
                              |> fromList

let difference left right = left.elements 
                            |> List.filter (fun x -> right.elements 
                                                     |> (List.contains <| x)
                                                     |> not)
                            |> fromList

let isSubsetOf left right = left.elements
                            |> List.forall (fun x -> right.elements
                                                     |> (List.contains <| x))

let isDisjointFrom left right = intersection left right = empty

let isEqualTo left right =  size left = size right 
                            && (left |> toList, right |> toList) ||> List.forall2 (=)
        