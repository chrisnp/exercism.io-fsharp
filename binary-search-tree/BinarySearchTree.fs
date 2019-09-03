module BinarySearchTree

type Node = { left : Node option; data : int; right : Node option }
    // Data of int * Node option * Node option 

let left node  = node.left

let right node = node.right

let data node = node.data

let rec private insert value (tree : Node) = 
    function
    | None   -> Some <| { left = None; data = value; right = None }
    | Some(tree) when value <= tree.data -> 
        Some <| { tree with left  = insert value tree.left }
    | Some(tree) -> Some <| { tree with right = insert value tree.right)} 

let create items = 
    items
    |> List.fold (fun node data -> insert data node) None
    |> Option.get

let sortedData node = failwith "You need to implement this function."