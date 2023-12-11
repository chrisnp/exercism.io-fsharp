module RationalNumbers

type Rational = int * int

let gcd (a: int) (b: int): int = 
    let rec gcd a b = if a % b = 0 then b
                      else gcd b (a % b)
    if a > b then gcd a b else gcd b a

let reduce (r: Rational): Rational = 
    let (a, b) = r 
    if a = 0 then (0, 1)
    else
        let a, b, gcd = if b < 0 then -a, -b, gcd -a -b 
                        else a, b, gcd a b
        if gcd = 1 then (a, b) 
        else (a / gcd, b / gcd) 

let create (numerator: int) (denominator: int): Rational =
    (numerator, denominator) |> reduce

let add (r1: Rational) (r2: Rational): Rational = 
    let (a1, b1), (a2, b2) = r1, r2
    (a1 * b2 + a2 * b1, b1 * b2) |> reduce

let sub (r1: Rational) (r2: Rational): Rational = 
    let (a1, b1), (a2, b2) = r1, r2
    (a1 * b2 - a2 * b1, b1 * b2) |> reduce

let mul (r1: Rational) (r2: Rational): Rational = 
    let (a1, b1), (a2, b2) = r1, r2
    (a1 * a2, b1 * b2) |> reduce

let div (r1: Rational) (r2: Rational): Rational = 
    let (a2, b2) = r2
    if b2 = 0 then failwith "Division by 0"
    else mul r1 (b2, a2) |> reduce

let abs (r: Rational): Rational =
    let (a, b) = r
    let apos = if a < 0 then -1 * a else a
    let bpos = if b < 0 then -1 * b else b
    (apos, bpos) |> reduce

let exprational (n: int) (r: Rational): Rational = 
    let (a, b) = r 
    if n = 0 then (1, 1)
    elif a = 0 then (0, b)
    else 
        let a, b, n = 
            if n > 0 then a, b, n else b, a, -n
        (pown a n, pown b n) |> reduce

let expreal (r: Rational) (n: int): float = 
    let (a, b) = r
    (float n) ** (float a / float b)
