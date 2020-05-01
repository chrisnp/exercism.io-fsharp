module BeerSong

let private stanza num = 
    match num with
    | 0 -> 
        ["No more bottles of beer on the wall, no more bottles of beer."; 
         "Go to the store and buy some more, 99 bottles of beer on the wall."]
    | 1 -> 
        ["1 bottle of beer on the wall, 1 bottle of beer."; 
         "Take it down and pass it around, no more bottles of beer on the wall."]
    | 2 -> 
        ["2 bottles of beer on the wall, 2 bottles of beer."; 
         "Take one down and pass it around, 1 bottle of beer on the wall."]
    | _ -> 
        [sprintf 
            "%d bottles of beer on the wall, %d bottles of beer." 
            num num;
         sprintf 
            "Take one down and pass it around, %d bottles of beer on the wall." 
            (num - 1)]

let recite (startBottles: int) (takeDown: int) = 
    [startBottles .. -1 .. startBottles - takeDown + 1] 
    |> List.map (stanza) 
    |> List.reduce (fun x y -> x @ [""] @ y)