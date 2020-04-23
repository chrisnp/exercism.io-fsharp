module BinarySearchTree

type Node = { left : Node option; 
              data : int; 
              right : Node option }
    
let left node  = node.left

let right node = node.right

let data node = node.data

let create items = 
    let rec insert items =
        match items with 
        | [] -> 
            None
        | x::xs -> 
            Some { left  = 
                    xs 
                    |> List.filter(fun y -> y <= x) 
                    |> insert;
                   data  = x;
                   right = 
                    xs 
                    |> List.filter(fun y -> y >  x) 
                    |> insert }
    match insert items with 
    | None -> failwith "" 
    | Some node -> node

let rec sortedData node = 
    let rec sort node = 
        match node with
            | None -> 
                []
            | Some x -> 
                sort x.left @ [x.data] @ sort x.right
    node 
    |> Some 
    |> sort
