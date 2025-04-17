# Import book-to-market data, market cap data, Fama French Factor data and calculate daily betas

# Import packages ---------------------------------------------------------
library(tidyverse)
library(RSQLite)
library(scales)
library(slider)
library(furrr)
library(tidyfinance)
library(RPostgres)
library(dplyr)
library(lubridate)
library(data.table)
library(frenchdata)


# Daily betas -------------------------------------------------------------
# Download risk free rate data from FF -----------------------------------------------------------
df_ff3 <- tidyfinance::download_data_factors_ff("factors_ff_3_daily", "1993-01-05", "2024-12-31") |> 
  select(date, mkt_excess, risk_free) |>
  collect()

#tidyfinance::set_wrds_credentials(user = "job2506", password = "Daxvax-gihcu1-tecxat")
#crsp_daily <- download_data_crsp("crsp_daily", "2020-12-01", "2020-12-31")

crsp_daily_rf <- daily_crsp |> #combine the datasets
  mutate(date = as.Date(date),
  RET = as.numeric(RET)
  )|>
  left_join(df_ff3,
            by = "date" # Join Fama-French data on date
  ) |>
  mutate(
    permno = PERMNO
  ) |>
  select(permno, RET, risk_free, date)

# Prepare batches ---------------------------------------------------------
permnos <- crsp_daily_rf |>
  distinct(permno) |> 
  pull(permno)

# Subset
# crsp_daily_rf <- crsp_daily_rf |> 
# filter(date >= as.Date("2020-01-01") & date <= as.Date("2020-6-30"))

batch_size <- 500 #from Tidyfinance (to avoid memory issues)
batches <- ceiling(length(permnos) / batch_size)

# Define functions from TidyFinance ---------------------------------------
estimate_capm <- function(data, min_obs = 48) {
  data <- data |> drop_na(RET, mkt_excess)
  
  if (nrow(data) < min_obs) {
    return(NA_real_)
  }
  
  # Run CAPM regression: excess return on market excess return
  tryCatch({
    fit <- lm(RET - risk_free ~ mkt_excess, data = data)
    as.numeric(coef(fit)[2])
  }, error = function(e) {
    return(NA_real_)
  })
}

roll_capm_estimation <- function(data, months, min_obs) {
  data <- data |> arrange(date)
  
  # Estimate beta using a rolling monthly window
  betas <- slide_period_vec(
    .x = data,
    .i = data$date,
    .period = "month",
    .f = ~ estimate_capm(.x, min_obs = min_obs),
    .before = months - 1,
    .complete = FALSE
  )
  
  tibble(date = unique(floor_date(data$date, "month")), beta = betas)
}

# Calculate betas ---------------------------------------------------------
beta_daily <- list()

for (j in seq_len(batches)) {
  
  permno_batch <- permnos[((j - 1) * batch_size + 1):min(j * batch_size, length(permnos))]
  
  crsp_daily_sub <- crsp_daily_rf |> 
    filter(permno %in% permno_batch) |> 
    select(permno, date, RET) |> 
    collect()
  
  crsp_daily_sub_nested <- crsp_daily_sub |> 
    inner_join(df_ff3, by = "date") |> 
    mutate(date = floor_date(date, "month")) |> 
    nest(data = c(date, RET, mkt_excess, risk_free)) 
  
  beta_daily[[j]]  <- crsp_daily_sub_nested |> 
    mutate(beta_daily = future_map(
      data, ~ roll_capm_estimation(.x, months = 3, min_obs = 48)
    )) |> 
    unnest(beta_daily) |> 
    select(permno, date, beta_daily = beta) |> 
    drop_na()
  
  message("Batch ", j, " out of ", batches, " done (", percent(j / batches), ")\n")
}

# Save the file
beta_daily <- bind_rows(beta_daily)
saveRDS(beta_daily, "E:/Seminar/Beta/betaresults.rds")


# Book to market ratio ----------------------------------------------------
# Data is imported through a CSV file from the CRSP database
bookmarket <- read_csv("input/bookmarket.csv")
booktomarket <- bookmarket[, -c(2, 3)] #remove columns 

booktomarket <- booktomarket %>% 
  mutate(date = as.Date(public_date)) #set date


# Market cap --------------------------------------------------
# Data is imported through a CSV file from the CRSP database
market_cap <- read_csv("input/crsp_data.csv") %>%
  rename(permno = PERMNO) %>%
  mutate(
    date = as.Date(date),
    market_cap = abs(PRC) * SHROUT / 1000,  # in million USD
    week_id = format(as.Date(date), "%Y-W%V")
  ) %>%
  select(permno, date, market_cap, week_id) 

saveRDS(market_cap, "data/metrics/MarketCap.rds")


# Fama French Factors -----------------------------------------------------
# This script can be used to extract the necessary Fama French factors from the Kenneth French data library. This code extracts the 
# 3 standard FF factors and also the Momentum factor. Together, this creates the Fama-French-Carhart 4-factor model. 

# Create model ------------------------------------------------------------
# Download Fama-French 3 daily factors (market excess return and risk-free rate) 
df_rf <- tidyfinance::download_data_factors_ff("factors_ff_3_daily", "1993-01-05", "2024-12-31") |> 
  mutate(week = floor_date(date, "weeks", week_start = "Tuesday")) |> 
  group_by(week) |>
  summarize(
    across(c("risk_free"), ~prod(1 + .x) - 1),
    trading_days_in_week = n()  # Add the count of trading days (rows) per week
  )

df_ff3 <- tidyfinance::download_data_factors_ff("factors_ff_3_daily", "1993-01-05", "2024-12-31") |> 
  mutate(week = floor_date(date, "weeks", week_start = "Tuesday")) |> 
  group_by(week) |>
  summarize(across(c("mkt_excess", "smb", "hml"), ~prod(1 + .x) - 1))

# Momentum factor weekly
df_mm <- tidyfinance::download_data_factors_ff("factors_ff_momentum_factor_daily", "1993-01-05", "2024-12-31") |> 
  mutate(week = floor_date(date, "weeks", week_start = "Tuesday")) |> 
  group_by(week) |>
  summarize(across("mom", ~ prod(1 + .x) - 1))

# Combine all four factors into one dataframe
df_ffc4 <- left_join(df_ff3, df_mm, by = "week")
df_ffc4 <- left_join(df_ffc4, df_rf, by = "week")

# Add a week key (see DataWeekly.R)
df_ffc4$key <- week_key(df_ffc4$week)

# Save to git
saveRDS(df_ffc4, "data/metrics/FFC4.rds")

