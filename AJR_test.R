# This script can be used to calculate the Adjusted jump-ratio test
# NOTE: ik heb alleen de functies gedefinieerd, er moet nog effe wat code bijgezet worden die daadwerkelijk de functies input geeft 
# om de waardes te berekenen 

# Some numeric values 
v = 0.6090 
mu = sqrt(2 / pi)
M = 78 
delta = 1/M 

# Function to calculate the realized BiPower variation
BPV <- function(returns){
  abs_returns <- abs(returns)
  return(sum(abs_returns[-1] * abs_returns[-length(abs_returns)]))
} 

# Function to calculate the realized QuadPower variation
QPV <- function(returns){
  abs_returns <- abs(returns)
  return(M * sum(
    abs_returns[1:(length(abs_returns)-3)] *
      abs_returns[2:(length(abs_returns)-2)] *
      abs_returns[3:(length(abs_returns)-1)] *
      abs_returns[4:length(abs_returns)]
  ))
}

# Function to calculate the adjusted jump-ratio test statistic
daily_AJR_stat <- function(returns, QPV, BPV, RV, M, mu) {
  sqrt(M) / sqrt(max(1, QPV / (BPV^2)) * (mu^(-2) * BPV / RV - 1))
}
