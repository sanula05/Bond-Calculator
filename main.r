

# Parameters
Cupon_rate <- 0.08

FV <- 1000
PMT <- Cupon_rate * FV
Time <- 5       # In Years
Current_time <- 5
Intrest_rates <- c(0.07, 0.07, 0.07, 0.07, 0.07)
Intrest_rate <- Intrest_rates[1]
V <- 0
Value <- c()
VAM <- 1136.16
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

# Weighted Cashflow assuming anual payments/ number of periods = time


                        
# Net Present Value Cashflow

# Macaulay Duration
Macaulay_Duration <- function(VAM,Time,Current_time,PMT,Intrest_rate,Intrest_rates,Cupon_rate,V,FV){
  Fixed = (Time * PMT) / ((1+Intrest_rate) ^ Time)
  Macaulay_Duration <- Fixed
  for (i in 1:Time){
    Macaulay_Duration <- Macaulay_Duration + (Fixed + ((i*VAM)/(1+Intrest_rate)^i)) 
  }
  M_Duration <- Macaulay_Duration / Price_at_maturity(V, Current_time, PMT, Intrest_rates,FV)
  return(M_Duration)
}
Macaulay_Duration(VAM,Time,Current_time,PMT,Intrest_rate,Intrest_rates,Cupon_rate,V,FV)
# Convexity

# Duration Convexity 



# Option <- readline(prompt <- "Value of Bond:1
# Yeild to Maturity of Bond:2
# Premium Bond:3");
# Option <- as.integer(Option);

# system <- function(V, Time, PMT, Intrest_rates,FV,Cupon_rate,VAM,Solutions,Option){
#   if (Option == 1){
Price_at_maturity(V, Time, PMT, Intrest_rates,FV) 
#   }
#  else if (Option == 2){
#     YTM(VAM,FV,Time,Solutions,Cupon_rate)
#   }
#   else if (Option == 3){
#     Premium(PMT,FV,Cupon_rate,Time,Intrest_rates)
#   }
# }
# system(V, Time, PMT, Intrest_rates,FV,Cupon_rate,VAM,Solutions,Option)

