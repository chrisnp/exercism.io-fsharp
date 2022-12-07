module DndCharacter

open System

let modifier x = float (x - 10) / 2.0 |> Math.Floor |> int

let ability() = Seq.init 4 (fun _ -> Random().Next(6) + 1) 
                |> Seq.skip 1 
                |> Seq.fold(fun sum x -> sum + x) 0

type Character() =
    let strength: int = ability()
    let dexterity: int = ability()
    let constitution: int = ability()
    let intelligence: int = ability()
    let wisdom: int = ability()
    let charisma: int = ability()
    member __.Strength with get() = strength
    member __.Dexterity with get() =  dexterity
    member __.Constitution with get() = constitution
    member __.Intelligence with get() = intelligence
    member __.Wisdom with get() = wisdom
    member __.Charisma with get() = charisma
    member __.Hitpoints with get() = modifier(__.Constitution) + 10

let createCharacter() = Character()
