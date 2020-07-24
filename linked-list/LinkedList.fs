module LinkedList  

type Node<'T> = { payload : 'T 
                  mutable prevNode: 'T Node Option
                  mutable nextNode: 'T Node Option }

type LinkedList<'T> = { mutable front: 'T Node Option }

let mkLinkedList () = { front = None }

let addToEmpty newValue linkedList = 
    let newNode = 
        Some { payload = newValue 
               prevNode = None 
               nextNode = None }
    linkedList.front <- newNode


let pop linkedList = 
    match linkedList.front with
    | None -> 
        failwith "empty list"
    | Some x -> 
        let mutable p = x
        while p.nextNode <> None do 
            p <- p.nextNode.Value
        if p.prevNode <> None then 
            p.prevNode.Value.nextNode <- None
        else 
            linkedList.front <- None
        p.payload
    

let shift linkedList = 
    match linkedList.front with
    | None -> 
        failwith "empty list - nothing to shift"
    | Some x ->
        let mutable p = x
        if p.nextNode <> None then 
            p.nextNode.Value.prevNode <- None
            linkedList.front <- p.nextNode
        else
            linkedList.front <- None
        p.payload

let push newValue linkedList = 
    match linkedList.front with
    | None -> 
        linkedList.front <- Some { payload = newValue
                                   prevNode = None
                                   nextNode = None }
    | Some x ->
        let mutable p = x
        while p.nextNode <> None do
            p <- p.nextNode.Value
        p.nextNode <- Some { payload = newValue
                             prevNode = Some p
                             nextNode = None }

let unshift newValue linkedList = 
    match linkedList.front with
    | None -> 
        linkedList.front <- Some { payload = newValue
                                   prevNode = None
                                   nextNode = None }
    | Some x ->
        let mutable p = x
        p.prevNode <- Some { payload = newValue
                             prevNode = None
                             nextNode = Some p }
        linkedList.front <- p.prevNode