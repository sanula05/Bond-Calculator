# Parameters
Cupon_rate <- 0.08
FV <- 1000
PMT <- Cupon_rate * FV
Time <- 5         # In Years
Intrest_rates <- c(0.07, 0.07, 0.07, 0.07, 0.07)
V <- 0
Value <- c()
VAM <- 1041.002
Solutions <- c()

# Funtion to predict the intrest rates to replace the list
# Data from Bank of england
Rates <- read.csv("Historical_rates.csv",header=TRUE,sep = ",")



# Price calculator,Time against inttrest rates, Time against Price 
Price_at_maturity <- function(V, Time, PMT, Intrest_rates,FV){
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
YTM <- function(VAM,FV,Time,Solutions,Cupon_rate){ 
Num_co = (1 + (FV/PMT))
T_co = VAM / PMT
# Solutions <- c(Solutions,Num_co)
# for (i in 1:(Time-1)) {
# Solutions <- c(Solutions,1)
# }
# Solutions <- c(Solutions,T_co)
# print(Solutions)
# return(polyroot(Solutions)-1)
yeild_to_maturity <- (Cupon_rate * 1000 + (FV-VAM)/Time) / ((VAM + FV)/2)
return(yeild_to_maturity)
}



Price_at_maturity(V, Time, PMT, Intrest_rates,FV)  
Premium(PMT,FV,Cupon_rate,Time,Intrest_rates)
YTM(VAM,FV,Time,Solutions,Cupon_rate)
