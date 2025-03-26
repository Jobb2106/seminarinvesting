# This script is for the programming of the decomposition of the RV

# Data --------------------------------------------------------------------
# Laad de data uit GitHub
df <- readRDS("data/clean/returns_5m_all_subset.rds")

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

# Split the RV into positive and negative part
df$rv_neg <- sapply(df$returns_5m, rv_negative)
df$rv_pos <- sapply(df$returns_5m, rv_positive)


# Compute signed jump -------------------------------------------------------------

df$signed_jump <- (df$rv_pos - df$rv_neg)


# Relative signed jump ----------------------------------------------------

df$RSJ_day <- df$signed_jump / df$rv          


# RSJ per week ------------------------------------------------------------
library(dplyr)

# Bereken trailing RSJ avg voor laatste 5 dagen en zet andere dagen 0
# hier bereken in gelijk de average return trailing, weet niet hoe handig dat uiteindelijk is
# TOCHECK de details, zie documentje 
df <- df %>%
  mutate(date = as.Date(date)) %>%
  arrange(permno, date) %>%
  group_by(permno) %>%
  mutate(
    # Rolling mean over 5 days using base R rollapplyr (no zoo needed)
    RSJ_roll = zoo::rollapplyr(RSJ_day, width = 5, FUN = mean, fill = NA),
    returns_roll = zoo::rollapplyr(open_close_log_ret, width = 5, FUN = mean, fill = NA),
    
    # Only keep it if it's a Tuesday, otherwise 0
    RSJ_week = if_else(weekdays(date) == "Tuesday", RSJ_roll, 0),
    returns_week = if_else(weekdays(date) == "Tuesday", returns_roll, 0)
  ) %>%
  ungroup()



# Save to GitHub ----------------------------------------------------------
# Vooral relevant voor als we daadwerkelijk met grote set gaan werken
saveRDS(df, "data/clean/returns_5m_all_subset.rds")







