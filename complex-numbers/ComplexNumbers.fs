module ComplexNumbers

type ComplexNumber = { real: float; imag: float }

let create (real: float) (imaginary: float): ComplexNumber = 
    { real = real; imag = imaginary }

let mul (z1: ComplexNumber) (z2: ComplexNumber): ComplexNumber = 
    (z1.real * z2.real - z1.imag * z2.imag, 
     z1.imag * z2.real + z1.real * z2.imag) 
    ||> create

let add (z1: ComplexNumber) (z2: ComplexNumber): ComplexNumber = 
    (z1.real + z2.real, z1.imag + z2.imag) ||> create

let sub (z1: ComplexNumber) (z2: ComplexNumber): ComplexNumber = 
    (z1.real - z2.real, z1.imag - z2.imag) ||> create

let div (z1: ComplexNumber) (z2: ComplexNumber): ComplexNumber = 
    let quot = z2.real ** 2.0 + z2.imag ** 2.0
    ((z1.real * z2.real + z1.imag * z2.imag) / quot, 
     (z1.imag * z2.real - z1.real * z2.imag) / quot) 
    ||> create

let abs (z: ComplexNumber): float = 
    ( z.real ** 2.0 + z.imag ** 2.0 ) ** 0.5

let conjugate (z: ComplexNumber): ComplexNumber = create z.real -z.imag

let real (z: ComplexNumber): float = z.real

let imaginary (z: ComplexNumber): float = z.imag

let exp (z: ComplexNumber): ComplexNumber = 
    let xp   = z.real |> System.Math.Exp
    (xp * (z.imag |> System.Math.Cos), xp * (z.imag |> System.Math.Sin)) 
    ||> create
