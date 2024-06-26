module SecretHandshake

let private actions = 
    [
        fun protocol -> protocol @ ["wink"]
        fun protocol -> protocol @ ["double blink"]
        fun protocol -> protocol @ ["close your eyes"]
        fun protocol -> protocol @ ["jump"]
        fun protocol -> List.rev protocol
    ]

let commands number = 
    let handshake = 
        actions |> List.indexed
                |> List.filter (fun (i, _) -> (number &&& (1 <<< i)) <> 0)
                |> List.fold (fun acc (_, f) -> acc >> f) id
    [] |> handshake 