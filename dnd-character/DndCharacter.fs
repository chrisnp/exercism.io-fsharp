module DndCharacter


let modifier x =
    float (x - 10) / 2.0 |> System.Math.Floor |> int

let ability() = 
    List.init 4 (fun _ -> System.Random().Next(6) + 1)
    |> List.skip 1 
    |> List.sum

type DndCharacter() =
    let strength = ability()
    let dexterity = ability()
    let constitution = ability()
    let intelligence = ability()
    let wisdom = ability()
    let charisma = ability()
    
    member __.Strength with get() = strength
    member __.Dexterity with get() = dexterity
    member __.Constitution with get() = constitution
    member __.Intelligence with get() = intelligence
    member __.Wisdom with get() = wisdom
    member __.Charisma with get() = charisma
    member __.Hitpoints with get() = modifier(__.Constitution) + 10

