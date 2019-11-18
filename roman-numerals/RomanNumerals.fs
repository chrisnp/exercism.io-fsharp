module RomanNumerals

let rec roman arabicNumeral = 
    if arabicNumeral >= 1000 then 
        "M" + roman(arabicNumeral - 1000)
    elif arabicNumeral >= 900 then 
        "CM" + roman(arabicNumeral - 900)
    elif arabicNumeral >= 500 then 
        "D" + roman(arabicNumeral - 500)
    elif arabicNumeral >= 400 then 
        "CD" + roman(arabicNumeral - 400)
    elif arabicNumeral >= 100 then 
        "C" + roman(arabicNumeral - 100)
    elif arabicNumeral >= 90 then 
        "XC" + roman(arabicNumeral - 90)
    elif arabicNumeral >= 50 then 
        "L" + roman(arabicNumeral - 50)
    elif arabicNumeral >= 40 then 
        "XL" + roman(arabicNumeral - 40)
    elif arabicNumeral >= 10 then 
        "X" + roman(arabicNumeral - 10)
    elif arabicNumeral >= 9 then 
        "IX" + roman(arabicNumeral - 9)
    elif arabicNumeral >= 5 then 
        "V" + roman(arabicNumeral - 5)
    elif arabicNumeral >= 4 then 
        "IV" + roman(arabicNumeral - 4)
    elif arabicNumeral >= 1 then 
        "I" + roman(arabicNumeral - 1)
    else ""
    