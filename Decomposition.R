# This script is for the programming of the decomposition of the RV

# Data --------------------------------------------------------------------
# Laad de data uit GitHub
file_paths <- list.files("data/subset", pattern = "^filtered_\\d{4}-W\\d{2}\\.rds$", full.names = TRUE)

# dropped paths
dropped_files <- list.files("data/setdifference", pattern = "^dropped_data_\\d{4}-W\\d{2}\\.rds$", full.names = TRUE)

# Libraries ---------------------------------------------------------------
library(dplyr)
library(stringr)
library(lmtest)
library(sandwich)


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
dropped_results <- list()
spread_data <- list()

# Hierbij wil ik mijn officiele excuses aanbieden aan Jop die dit idee op het begin had. Het was een goed idee.
# Bereken weekly_returns & RSJ voor elke week en bereken ook weekly_returns voor bedrijven die eruit vallen
for (file in file_paths) {
  df <- readRDS(file)
  week_id <- str_remove(basename(file), "\\.rds$")
  clean_week_id <- str_remove(week_id, "^filtered_")
  
  # Berekeningen normaal
  df <- calculate_RSJ_day(df)
  df_week_summary <- summarise_week(df, week_id)
  df_week_summary$portfolio <- assign_portfolio(df_week_summary, "RSJ_week", n_portfolios = 5)
  weekly_results[[week_id]] <- df_week_summary
  
  # Berekeningen dropped
  dropped_file <- file.path("data/setdifference", paste0("dropped_data_", clean_week_id, ".rds"))
  if (file.exists(dropped_file)) {
    dropped_df <- readRDS(dropped_file)
    
    if (!"returns_week" %in% colnames(dropped_df)) {
      dropped_df <- calculate_RSJ_day(dropped_df)
      dropped_df <- summarise_week(dropped_df, clean_week_id)
      dropped_results[[clean_week_id]] <- dropped_df
    }
  }
}


# Portfolio Sorting -------------------------------------------------------
portfolio_performance <- list()
weekly_spreads <- list()

# Sanity
week_ids <- sort(names(weekly_results))

for (i in 1:(length(week_ids) - 1)) {
  current_week_id <- week_ids[i]
  next_week_id <- week_ids[i + 1]
  clean_next_id <- sub("^filtered_", "", next_week_id)
  
  current_week <- weekly_results[[current_week_id]]
  next_week <- weekly_results[[next_week_id]]
  dropped_next <- dropped_results[[clean_next_id]]
  
  # Next week returns
  joined <- add_next_week_return(
    current_week_df = current_week,
    next_week_df = next_week,
    dropped_df = dropped_results[[clean_next_id]]
  )
  
  # Optional post-processing steps:
  perf <- summarise_portfolios(joined)
  perf$week_id <- week_ids[i]
  
  # Store result
  portfolio_performance[[week_ids[i]]] <- perf
}


# t-value for high-low spread
performance_panel <- bind_rows(portfolio_performance)
model <- lm(spread ~ 1, data = performance_panel)
coeftest(model, vcov = NeweyWest)



