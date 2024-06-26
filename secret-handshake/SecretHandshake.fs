module SecretHandshake

let private actions = 
    [
        fun handshake -> handshake @ ["wink"]
        fun handshake -> handshake @ ["double blink"]
        fun handshake -> handshake @ ["close your eyes"]
        fun handshake -> handshake @ ["jump"]
        fun handshake -> List.rev handshake
    ]

let commands number = 
    actions 
    |> List.indexed
    |> List.filter (fun (idx, _) -> (number &&& (1 <<< idx)) <> 0)
    |> List.fold (fun acc (_, action) -> acc >> action) id 
    <| []
