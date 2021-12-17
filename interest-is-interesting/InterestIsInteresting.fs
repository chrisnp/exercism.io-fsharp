module InterestIsInteresting

let interestRate (balance: decimal): single =
   function 
   | b when b <  0m    -> 3.213f
   | b when b <  1000m -> 0.5f
   | b when b <  5000m -> 1.621f
   | b when b >= 5000m -> 2.475f
   | _ -> 0f
   <| balance

let interest (balance: decimal): decimal =
   let rate = (/) (balance |> interestRate |> decimal) 100m
   balance |> (*) rate

let annualBalanceUpdate(balance: decimal): decimal =
   balance 
   |> (+) (balance |> interest)

let amountToDonate(balance: decimal) (taxFreePercentage: float): int =
   let donationRate = (/) (taxFreePercentage |> decimal) 100m
   if balance > 0m then
      balance 
      |> (*) 2m 
      |> (*) donationRate
      |> int
   else 0
