module RobotName

open System

type Robot = { name : string }

let generateName() = 
    let alpha = ['A'..'Z']
    let random = Random()
    string (alpha.[random.Next(26)]) + 
    string (alpha.[random.Next(26)]) + 
    (string (random.Next(999)))

let mutable usedNames : string list = []

let rec mkRobot() = 
    let newRobot = generateName()
    if List.contains newRobot usedNames then
        mkRobot()
    else 
        usedNames <- newRobot::usedNames
        { name = newRobot } 

let name robot = robot.name

let reset robot = mkRobot() 
