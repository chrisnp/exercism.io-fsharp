module Triangle


let private isATriangle triangle =
    let [a; b; c] = triangle
    let nonZero = List.sum triangle <> 0.0
    let inequality =
        a + b >= c && a + c >= b && b + c >= a
    let degenerate = 
        a + b = c || a + c = b || b + c = a
    nonZero && not degenerate && inequality


let private distinctSides triangle = triangle |> List.distinct |> List.length

let equilateral triangle = (triangle |> isATriangle) && (triangle |> distinctSides = 1)

let isosceles triangle = (triangle |> isATriangle) && (triangle |> distinctSides <= 2)

let scalene triangle = (triangle |> isATriangle) && (triangle |> distinctSides = 3)