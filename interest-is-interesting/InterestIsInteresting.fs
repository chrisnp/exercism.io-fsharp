module InterestIsInteresting

let interestRate (balance: decimal): single =
   balance |>
   function 
   | b when b <  0m    -> 3.213f
   | b when b <  1000m -> 0.5f
   | b when b <  1000m -> 1.621f
   | b when b >= 5000m -> 2.475f
   | _ -> 0f

let interest (balance: decimal): decimal =
   (*) balance ((balance |> interestRate) |> decimal) 

let annualBalanceUpdate(balance: decimal): decimal =
   (+) balance (balance |> interest)

let amountToDonate(balance: decimal) (taxFreePercentage: float): int =
   if balance > 0m then
      let donation = (*) 2m balance ((0.01f * taxFreePercentage) |> decimal)
      donation |> floor |> int
   else 
      0
