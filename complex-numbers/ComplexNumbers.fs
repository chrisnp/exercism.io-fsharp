module ComplexNumbers

type ComplexNumber = {re: float; im: float}

let create real imaginary = { re = real; im = imaginary} 

let mul z1 z2 = 
    let real = z1.re * z2.re - z1.im * z2.im
    let imag = z1.im * z2.re + z1.re * z2.im
    create real imag

let add z1 z2 = 
    let real = z1.re + z2.re
    let imag = z1.im + z2.im
    create real imag

let sub z1 z2 = 
    let real = z1.re - z2.re
    let imag = z1.im - z2.im
    create real imag

let div z1 z2 = 
    let quot = z2.re ** 2.0 + z2.im ** 2.0
    let real = (z1.re * z2.re + z1.im * z2.im) / quot
    let imag = (z1.im * z2.re - z1.re * z2.im) / quot
    create real imag

let abs z = ( z.re ** 2.0 + z.im ** 2.0 ) ** 0.5

let conjugate z = create z.re -z.im

let real z = z.re

let imaginary z = z.im

let exp z = 
    let xp   = z.re |> System.Math.Exp
    let real = xp * (z.im |> System.Math.Cos) 
    let imag = xp * (z.im |> System.Math.Sin) 
    create real imag