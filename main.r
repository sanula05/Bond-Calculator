# Parameters
Cupon_rate <- 0.08
FV <- 1000
PMT <- Cupon_rate * FV
Time <- 5
# how may times a year does the bond payout
# Payment_time_step <- 2
# Time <- Time * Payment_time_step
Intrest_rates <- c(0.07, 0.07, 0.07, 0.07, 0.07)
V <- 0
Value <- c()
VAM <- 1000

# Build Funtion to predict the intrest rates to replace the list
# Data from Bank of england
Rates <- read.csv("Historical_rates.csv",header=TRUE,sep = ",")
# print(Rates)





# Price calculator,Time against inttrest rates, Time against Price 
Price_at_maturity <- function(V, Time, PMT, Intrest_rates,FV,Cupon_rate){
  for (i in 1:Time)
    if (i != Time) { 
    V <- V + PMT / (1 + Intrest_rates[i])^i
    } else {
    V <- V + (PMT + FV) / ((1+Intrest_rates[i])^i)
    }
  return(V)
}
# Determines whather the bond is premium
Premium <- function(PMT,FV,Cupon_rate,Time,Intrest_rates){
  avg_rate <- mean(Intrest_rates)
  Prm = PMT/FV
  if (Prm > avg_rate){
    return("Premium Bond")
    }
  else{
    return("not Premium Bond")
    }
  }
#Yeild to maturity
YTM <- function(VAM,FV,Time,PMT){ 
  poly_YTM <- c(1 + FV)
  {for (i in 1:Time-1)
    poly_YTM <- poly_YTM + 1}
  poly_YTM <- (-VAM/PMT)
  print(list(poly_YTM))
  Solutions = polyroot(poly_YTM)
  print(Solutions)
  }
Price_at_maturity(V, Time, PMT,FV,Cupon_rate)  
Premium(PMT,FV,Cupon_rate,Time,Intrest_rates)
YTM(VAM,FV,Time,PMT)
# Yeild to maturity and Callable to add