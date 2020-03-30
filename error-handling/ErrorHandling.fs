module ErrorHandling

let handleErrorByThrowingException() = 
    raise(System.Exception())

let handleErrorByReturningOption (input:string) = 
    match System.Char.IsDigit(char input) with 
    | true -> 
        Some (int input)
    | _ -> 
        None

let handleErrorByReturningResult (input:string) = 
    match System.Char.IsDigit(char input) with
    | true -> 
        Ok (int input)
    | _ -> 
        Error "Could not convert input to integer"

let bind switchFunction twoTrackInput = 
    match twoTrackInput with
    | Ok x    -> 
        switchFunction x
    | Error e -> 
        Error e

let cleanupDisposablesWhenThrowingException resource = 
    using (resource) ( fun _ -> 
                            raise(System.Exception()))