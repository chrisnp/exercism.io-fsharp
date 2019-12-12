﻿module Triangle

let private isATriangle triangle =
    let nonZero = List.sum triangle <> 0.0
    let inequality =
        let [a; b; c] = triangle
        a + b >= c && a + c >= b && b + c >= a
    inequality && nonZero

let private distinctSides triangle = triangle 
                                     |> List.distinct 
                                     |> List.length

let equilateral triangle = (triangle |> isATriangle) && 
                           (triangle |> distinctSides = 1)

let isosceles triangle = (triangle |> isATriangle) && 
                         (triangle |> distinctSides <= 2)

let scalene triangle = (triangle |> isATriangle) && 
                       (triangle |> distinctSides = 3)