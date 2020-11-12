module DndCharacter


let modifier x =
    float (x - 10) / 2.0 
    |> System.Math.Floor 
    |> int

let ability() = 
    List.init 4 (fun _ -> 
                    System.Random()
                          .Next(6) + 1)
    |> List.skip 1 
    |> List.sum

type Character =
    { Strength: int
      Dexterity: int
      Constitution: int
      Intelligence: int
      Wisdom: int
      Charisma: int }
    member this.Hitpoints = 
        modifier (this.Constitution) + 10
    
    
let createCharacter() : Character =  
    { Strength = ability ()
      Dexterity = ability ()
      Constitution = ability ()
      Intelligence = ability ()
      Wisdom = ability ()
      Charisma = ability () }
    

