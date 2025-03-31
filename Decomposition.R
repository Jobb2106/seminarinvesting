# This script is for the programming of the decomposition of the RV

# Data --------------------------------------------------------------------
# Laad de data uit GitHub
file_paths <- list.files("data/subset", pattern = "^filtered_\\d{4}-W\\d{2}\\.rds$", full.names = TRUE)


# Libraries ---------------------------------------------------------------
library(dplyr)
library(stringr)


# Negative/positive realized volatility -----------------------------------
# Functions for RV calculations
rv_negative <- function(returns) {
  neg_returns <- returns[returns < 0]
  sum(neg_returns^2)
}

rv_positive <- function(returns) {
  pos_returns <- returns[returns > 0]
  sum(pos_returns^2)
}

calculate_RSJ_day <- function(df) {
  df$rv_neg <- sapply(df$returns_5m, rv_negative)
  df$rv_pos <- sapply(df$returns_5m, rv_positive)
  
  df %>%
    mutate(
      signed_jump = rv_pos - rv_neg,
      RSJ_day = signed_jump / rv,
      date = as.Date(date)
    )
}

summarise_week <- function(df, week_id) {
  df %>%
    group_by(permno) %>%
    summarise(
      RSJ_week = mean(RSJ_day, na.rm = TRUE),
      returns_week = mean(open_close_log_ret, na.rm = TRUE),
      n_days = n(),
      .groups = "drop"
    ) %>%
    mutate(week_id = week_id)
}


# Calculations ------------------------------------------------------------
weekly_results <- list()
portfolio_performance <- list()
spread_data <- list()

# Hierbij wil ik mijn officiele excuses aanbieden aan Jop die dit idee op het begin had. Het was een goed idee.
for (file in file_paths) {
  df <- readRDS(file)
  week_id <- str_remove(basename(file), "\\.rds$")
  
  # Berekeningen
  df <- calculate_RSJ_day(df)
  df_week_summary <- summarise_week(df, week_id)
  df_week_summary$portfolio <- assign_portfolio(df_week_summary, "RSJ_week", n_portfolios = 5)
  
  weekly_results[[week_id]] <- df_week_summary
}


# Portfolio Sorting -------------------------------------------------------
portfolio_performance <- list()
weekly_spreads <- list()

# Voor de zekerheid
week_ids <- sort(names(weekly_results))

for (i in 1:(length(week_ids) - 1)) {
  current_week <- weekly_results[[week_ids[i]]]
  next_week <- weekly_results[[week_ids[i + 1]]]
  
  joined <- add_next_week_return(current_week, next_week)
  
  perf <- summarise_portfolios(joined)
  perf$week_id <- week_ids[i]
  portfolio_performance[[week_ids[i]]] <- perf
  
  spread <- calculate_weekly_spreads(joined, week_ids[i])
  weekly_spreads[[week_ids[i]]] <- spread
}




