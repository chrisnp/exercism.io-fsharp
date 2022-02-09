module Clock

type Clock = Clock of int

let private dayMins = 1440

let private modDay = (%) >> (|>) dayMins

let private canonical time = 
    if time >= 0 then modDay(time) else modDay(time) + dayMins

let create hours minutes = (+) ((*) 60 hours) minutes |> canonical

let add minutes clock = (0, clock + minutes) ||> create 

let subtract minutes clock = (0, clock - minutes) ||> create 

let display clock = $"%02i{clock / 60}:%02i{clock % 60}"