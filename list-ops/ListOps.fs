module ListOps

let inline (><) f x y = f y x 

let rec foldl folder state list = 
   match list with
   | [] -> state
   | x::xs -> foldl folder (folder state x) xs

let length list = 
   foldl (fun state x -> state + 1) 0 list

let reverse list = 
   foldl (fun state x -> x::state) [] list

let foldr folder state list = 
   foldl ((><) folder) state <| reverse list

let map f list = 
   foldr (fun x state -> (f x)::state) [] list

let filter f list = 
   foldr (fun x state -> 
                  if f x then x::state 
                         else state) [] list

let append xs ys = 
   foldr (fun x state -> x::state) ys xs

let concat xs = 
   foldr append [] xs
