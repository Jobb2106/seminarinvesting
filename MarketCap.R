# This script can be used to calculate monthly market cap values, which we can use for the value-weighted portfolios and Fama-Macbeth regression 
# NOTE: To run this script, up until now, fix the path. This can probably be improved by Job or Thor by uploading it to Git I guess? 
# You can donwload the file from Jop's WRDS account. It has query id 9705291


# Import packages ---------------------------------------------------------
library(readr)
library(dplyr)


# Import data -------------------------------------------------------------
input = "input/crsp_data.csv"
df <- read_csv(path)


# Calculate monthly market cap --------------------------------------------
# Add a column to the dataframe with the market cap 
df$market_cap <- abs(df$PRC) * df$SHROUT / 1000  # in millions USD

# Group the dataframe by date 
df_grouped <- df %>%
  ungroup() %>%
  arrange(date) %>%       # <-- this sorts the data
  group_by(date)


# Save --------------------------------------------------------------------
saveRDS(df_grouped, file.path("/data/metrics/", "MarketCap.rds"))

