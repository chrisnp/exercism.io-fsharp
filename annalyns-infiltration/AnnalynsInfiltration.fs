module AnnalynsInfiltration

let canFastAttack (knightIsAwake: bool): bool = 
    if knightIsAwake then false else true


let canSpy (knightIsAwake: bool) (archerIsAwake: bool) (prisonerIsAwake: bool): bool =
    if knightIsAwake then true elif archerIsAwake then true else prisonerIsAwake


let canSignalPrisoner (archerIsAwake: bool) (prisonerIsAwake: bool): bool =
    match (archerIsAwake, prisonerIsAwake) with
        | (false, true) -> true
        | _ -> false

let canFreePrisoner (knightIsAwake: bool) (archerIsAwake: bool) (prisonerIsAwake: bool) (petDogIsPresent: bool): bool =
    let archerIsSleeping: bool = not <| archerIsAwake
    let knightIsSleeping: bool = not <| knightIsAwake
    archerIsSleeping && (petDogIsPresent || (prisonerIsAwake && knightIsSleeping))
