module RobotName

open System

let generateName() = 
    let alpha = 
        ['A'..'Z']
    let random = 
        Random()
    string (alpha.[random.Next(26)]) + 
    string (alpha.[random.Next(26)]) + 
    (string (random.Next(999)))

type Robot = { name : string }

let mkRobot() = 
    { name = generateName() }
let name robot = 
    robot.name
let reset robot = 
    { robot with name = generateName() } 

