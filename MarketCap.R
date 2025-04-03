# This script can be used to calculate monthly market cap values, which we can use for the value-weighted portfolios and Fama-Macbeth regression 
# NOTE: To run this script, up until now, fix the path. This can probably be improved by Job or Thor by uploading it to Git I guess? 
# You can donwload the file from Jop's WRDS account. It has query id 9705291

library(readr)
library(dplyr)

path = "/Users/Timo/Downloads/crsp_data.csv"
df <- read_csv(path)

# Add a column to the dataframe with the market cap 
df$market_cap <- abs(df$PRC) * df$SHROUT / 1000  # in millions USD

# Group the dataframe by date 
df_grouped <- df %>%
  ungroup() %>%
  arrange(date) %>%       # <-- this sorts the data
  group_by(date)

# Perhaps add the following code here to write this data back into a CSV file? (of course change the path) 
# write_csv(df_grouped, "/Users/Timo/Downloads/df_grouped.csv")
