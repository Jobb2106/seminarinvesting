# This script can be used to extract the necessary Fama French factors from the Kenneth French data library. This code extracts the 
# 3 standard FF factors and also the Momentum factor. Together, this creates the Fama-French-Carhart 4-factor model. 

# Import packages ---------------------------------------------------------
library(tidyfinance)   # For downloading Fama-French factor data
library(dplyr)         # For piping (%>%), mutate(), group_by(), summarize(), across()
library(lubridate)
library(frenchdata)


# Create model ------------------------------------------------------------
df_ff3 <- tidyfinance::download_data_factors_ff("factors_ff_3_daily", "1993-01-05", "2024-12-31") |> 
  mutate(week = floor_date(date, "weeks", week_start = "Tuesday")) |> 
  group_by(week) |>
  summarize(across(c("mkt_excess", "smb", "hml"), ~prod(1 + .x) - 1))

df_mm <- tidyfinance::download_data_factors_ff("factors_ff_momentum_factor_daily", "1993-01-04", "2024-12-31") |> 
  mutate(week = floor_date(date, "weeks", week_start = "Tuesday")) |> 
  group_by(week) |>
  summarize(across("mom", ~ prod(1 + .x) - 1))

df_ffc4 <- left_join(df_ff3, df_mm, by = "week")

df_ffc4$key <- week_key(df_ffc4$week)

# Save to Git -------------------------------------------------------------
saveRDS(df_ffc4, "data/metrics/FFC4.rds")
