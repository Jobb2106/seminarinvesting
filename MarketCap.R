# This script can be used to calculate monthly market cap values, which we can use for the value-weighted portfolios and Fama-Macbeth regression 


# Import packages ---------------------------------------------------------
library(readr)
library(dplyr)


# Import data -------------------------------------------------------------
market_cap <- read_csv("input/crsp_data.csv") %>%
  rename(permno = PERMNO) %>%
  mutate(
    date = as.Date(date),
    market_cap = abs(PRC) * SHROUT / 1000,  # in miljoenen USD
    week_id = format(as.Date(date), "%Y-W%V")
  ) %>%
  select(permno, date, market_cap, week_id) 


# Save --------------------------------------------------------------------
saveRDS(market_cap, "data/metrics/MarketCap.rds")


