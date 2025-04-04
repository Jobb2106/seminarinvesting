# This script can be used to calculate monthly market cap values, which we can use for the value-weighted portfolios and Fama-Macbeth regression 


# Import packages ---------------------------------------------------------
library(readr)
library(dplyr)


# Import data -------------------------------------------------------------
market_cap <- read_csv("input/crsp_data.csv") %>%
  rename(permno = PERMNO) %>%
  mutate(
    date = as.Date(date),
    market_cap = abs(PRC) * SHROUT / 1000  # in miljoenen USD
  ) %>%
  select(permno, date, market_cap) 


# Save --------------------------------------------------------------------
saveRDS(market_cap, "data/metrics/MarketCap.rds")


