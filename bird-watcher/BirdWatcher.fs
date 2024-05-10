module BirdWatcher

let lastWeek: int[] =
   [| 0; 2; 5; 3; 7; 8; 4 |]

let yesterday(counts: int[]): int =
  counts |> Seq.item 5

let total(counts: int[]): int =
  counts |> Seq.sum

let dayWithoutBirds(counts: int[]): bool =
  counts |> Seq.contains 0

let incrementTodaysCount(counts: int[]): int[] =
  counts.[counts.Length - 1] <- counts.[counts.Length - 1] + 1
  counts 

let unusualWeek(counts: int[]): bool =
  let isOdd n = n % 2 = 1
  let tuple a b = (a, b)
  counts |> Seq.mapi tuple 
         |> Seq.filter (fst >> isOdd) 
         |> Seq.map snd 
         |> Seq.forall ((=) 0)
  ||
  counts |> Seq.mapi tuple 
         |> Seq.filter (fst >> isOdd) 
         |> Seq.map snd 
         |> Seq.forall ((=) 10)
  ||
  counts |> Seq.mapi tuple 
         |> Seq.filter (fst >> isOdd >> not) 
         |> Seq.map snd 
         |> Seq.forall ((=) 5)
