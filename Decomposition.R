# This script is for the programming of the decomposition of the RV

# Negative/positive realized volatility -----------------------------------
# Compute negative realized volatility for each stock
rv_negative <- function(returns) {
  neg_returns <- returns[returns < 0]
  sum(neg_returns^2)
}

# Compute positive realized volatility for each stock
rv_positive <- function(returns) {
  pos_returns <- returns[returns > 0]
  sum(pos_returns^2)
}

rv_neg <- sapply(fivereturn, rv_negative)
rv_pos <- sapply(fivereturn, rv_positive)


# Plot
plot(rv_neg, type = "h", col = "blue",
     main = "Realized negative volatility for each stock",
     xlab = "Stock", ylab = "Volatility")

# Plot
plot(rv_pos, type = "h", col = "magenta",
     main = "Realized positive volatility for each stock",
     xlab = "Stock", ylab = "Volatility")


# Compute signed jump -------------------------------------------------------------
signed_jump <- (rv_pos - rv_neg)


# Relative signed jump ----------------------------------------------------
rvtotal <- function(returns) {
  sum(returns^2)
}

rv_total <- sapply(fivereturn, rvtotal)
RSJ <- signed_jump / rv_total





