# This script is for the programming of realized volatility
library(ggplot2)

# Function
realized_variance <- function(day_returns) {
  sum(day_returns^2)
}

rv <- sapply(fivereturn, realized_variance)
rv_ts <- ts(rv)


# Plot returns
all_returns <- unlist(fivereturn)
time_index <- 1:length(all_returns)

plot(time_index, all_returns, type = "l", col = "blue",
     main = "5-Minute Returns Over Time",
     xlab = "Time Index", ylab = "Return")




