module ListOps

let rec foldl folder state list = 
   match list with
   | [] -> state
   | x::xs -> foldl folder (folder state x) xs

let rec length list = 
   foldl (fun state x -> state + 1) 0 list

let reverse list = 
   foldl (fun state x -> x::state) [] list

let rec foldr folder state list = 
   let (><) f x y = f y x 
   foldl ((><) folder) state <| (reverse list)

let map f list = 
   foldr (fun x state -> f x :: state) [] list

let filter f list = 
   [for x in list do if f x then yield x]

let append xs ys = 
   foldr (fun x state -> x :: state) ys xs

let concat xs = 
   foldr append [] xs
