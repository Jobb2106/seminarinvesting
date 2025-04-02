#--------------------------------------------------------------------------------------------------------------------------------------
# Hieronder staan de functies die gebruikt kunnen worden om onze eigen "negative return jump ratio" te berekenen. Misschien is het 
# handiger om dit in een eigen script te zetten? Moeten jullie (Thor en Job) maar even bepalen wat het beste is hiervoor. Opnieuw heb ik 
# alleen de functies gedefinieerd, er moet dus nog even wat code toegevoegd worden die daadwerkelijk de functies gebruikt en output 
# genereerd. 

# Compute negative realized volatility for each stock, per day. 
# Deze functie heb ik gekopieerd van decomposition.R. Als we die waarden daar gewoon kunnen accessen in deze script is de functie onnodig.
rv_negative <- function(returns) {
  neg_returns <- returns[returns < 0]
  sum(neg_returns^2)
}

# Function to calculate the negative bipower variation 
BPV_negative <- function(returns){
  abs_returns <- abs(returns)
  return(sum(abs_returns[-1] * abs_returns[-length(abs_returns)] * (returns(-1) < 0)))
} 

# Function to calculate negative JV 
JV_negative <- function(rv_negative, BPV_negative, mu){
  max(RV_negative - mu^(-2) * BPV_negative, 0)
}

# Function to calculate negative JR 
JR_negative <- function(JV_negative, rv_negative){
  JV_negative / rv_negative 
}