# This script is for the programming of the decomposition of the RV

# Negative/positive realized volatility -----------------------------------
# Compute negative realized volatility for each stock, per day
rv_negative <- function(returns) {
  neg_returns <- returns[returns < 0]
  sum(neg_returns^2)
}

# Compute positive realized volatility for each stock, per day
rv_positive <- function(returns) {
  pos_returns <- returns[returns > 0]
  sum(pos_returns^2)
}

# TODO: per stock per dag, dus een loopje maken voor intraday returns
rv_neg <- sapply(fivereturn, rv_negative)
rv_pos <- sapply(fivereturn, rv_positive)


# Plot
# Ter simpele controle of het klopt, maar hoeft niet eigenlijk
plot(rv_neg, type = "h", col = "blue",
     main = "Realized negative volatility for each stock",
     xlab = "Stock", ylab = "Volatility")

# Plot
plot(rv_pos, type = "h", col = "magenta",
     main = "Realized positive volatility for each stock",
     xlab = "Stock", ylab = "Volatility")


# Compute signed jump -------------------------------------------------------------
# TODO: per day (per stock)
signed_jump <- (rv_pos - rv_neg)


# Relative signed jump ----------------------------------------------------
# TODO: per day (per stock)
rvtotal <- function(returns) {
  sum(returns^2)
}

rv_total <- sapply(fivereturn, rvtotal)
rv_check <- rv_pos + rv_neg               # should be that rv_total = rv_check

RSJ_day <- signed_jump / rv_total             # per day


# RSJ per week ------------------------------------------------------------\
library(zoo) # install.packages("zoo")

# Create zoo object
RSJ_zoo <- zoo(df$RSJ_day, order.by = df$date)

# Rolling mean over 5 trading days (ending on that day)
RSJ_roll <- rollapply(RSJ_zoo, width = 5, FUN = mean, align = "right", fill = NA)

# Filter only values where the end date is a Tuesday
RSJ_roll_tuesday <- RSJ_roll[weekdays(index(RSJ_roll)) == "Tuesday"]

RSJ_roll_tuesday








