module RobotName

open System

let generateName() = 
    string (['A'..'Z'].[Random().Next(26)]) + 
    string (['A'..'Z'].[Random().Next(26)]) + 
    string (Random().Next(999))

type Robot = { name : string }

let mkRobot() = { name = generateName() }
let name robot = robot.name
let reset robot = { robot with name = generateName() } 

