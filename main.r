# Parameters
Inital_intrest_rate <- 0.08
FV <- 1000
PMT <- Inital_intrest_rate * FV
Time <- 5
# how may times a year does the bond payout
# Payment_time_step <- 2
# Time <- Time * Payment_time_step
Intrest_rates <- c(0.08, 0.07, 0.07, 0.07, 0.07)
V <- 0
Value <- c()

# Price calculator,Time against inttrest rates, Time against Price 
Price_at_maturity <- function(V, Time, PMT, Intrest_rates,FV,Inital_intrest_rate,value){
  for (i in 1:Time)
    if (i != Time) { 
    V <- V + PMT / (1 + Intrest_rates[i])^i
    } else {
    V <- V + (PMT + FV) / ((1+Intrest_rates[i])^i)
    }
  return(list(V))
}
Price_at_maturity(V, Time, PMT, Intrest_rates,FV,Inital_intrest_rate)  

