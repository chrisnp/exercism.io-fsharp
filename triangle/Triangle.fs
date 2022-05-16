module Triangle

let private isProper triangle =
    let allSidesNonZero = 
        List.contains 0.0 triangle |> not
    let triangleInequality =
        2.0 * (List.max triangle) < (List.sum triangle)
    allSidesNonZero && triangleInequality

let private distinctSides triangle = 
    triangle |> List.distinct |> List.length

let equilateral triangle = 
    (triangle |> isProper) && (triangle |> distinctSides = 1)

let isosceles triangle = 
    (triangle |> isProper) && (triangle |> distinctSides <= 2)

let scalene triangle = 
    (triangle |> isProper) && (triangle |> distinctSides = 3)