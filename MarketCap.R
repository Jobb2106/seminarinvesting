# This script can be used to calculate monthly market cap values, which we can use for the value-weighted portfolios and Fama-Macbeth regression 


# Import packages ---------------------------------------------------------
library(readr)
library(dplyr)


# Import data -------------------------------------------------------------
df <- read_csv("input/crsp_data.csv")


# Calculate monthly market cap --------------------------------------------
# Add a column to the dataframe with the market cap 
df$market_cap <- abs(df$PRC) * df$SHROUT / 1000  # in millions USD

# Group the dataframe by date 
df_grouped <- df %>%
  ungroup() %>%
  arrange(date) %>%       # <-- this sorts the data
  group_by(date)


# Save --------------------------------------------------------------------
saveRDS(df_grouped, "data/metrics/MarketCap.rds")

