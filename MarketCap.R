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

# Toevoegen aan Data ------------------------------------------------------
# Dit duurt denk ik te lang om te runnen dus ik zoek een andere oplossing
library(dplyr)
library(lubridate)
library(readr)

file_paths <- list.files("data/subset", pattern = "^filtered_\\d{4}-W\\d{2}\\.rds$", full.names = TRUE)
test_files <- head(file_paths, 5)

for (file in test_files) {
  df_week <- readRDS(file) %>%
    mutate(date = as.Date(date))  # Zorg dat datumformaat overeenkomt
  
  
  # 3. Voeg de meest recente beschikbare market_cap toe per permno en date
  df_week_with_mc <- df_week %>%
    rowwise() %>%
    mutate(
      market_cap = df %>%
        filter(permno == .data$permno, date <= .data$date) %>%
        slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
        pull(market_cap)
    ) %>%
    ungroup()
  
  # 4. Opslaan (overschrijven)
  saveRDS(df_week_with_mc, file)
}


<<<<<<< HEAD

# Save simpel -------------------------------------------------------------
saveRDS(market_cap, "data/metrics/MarketCap.rds")


=======
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

>>>>>>> b795d45c49f0914bf766ea7dfdce7766b7ea2c0f
