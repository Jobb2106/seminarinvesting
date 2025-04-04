# This script is for the programming of the decomposition of the RV

# Data --------------------------------------------------------------------
# Laad de data uit GitHub
file_paths <- list.files("data/subset", pattern = "^filtered_\\d{4}-W\\d{2}\\.rds$", full.names = TRUE)

# dropped paths
dropped_files <- list.files("data/setdifference", pattern = "^dropped_data_\\d{4}-W\\d{2}\\.rds$", full.names = TRUE)

# FFC4 factors
ffc4_factors <- readRDS("data/metrics/FFC4.rds") %>%
  mutate(week = as.Date(week))


# Libraries ---------------------------------------------------------------
library(dplyr)
library(stringr)
library(lmtest)
library(sandwich)
library(purrr)


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
      RSJ_day = signed_jump / (rv_pos + rv_neg),
      date = as.Date(date)
    )
}

summarise_week <- function(df, week_id) {
  df %>%
    group_by(permno) %>%
    summarise(
      RSJ_week = mean(RSJ_day, na.rm = TRUE),
      returns_week = sum(open_close_log_ret, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(week_id = week_id)
}

# Calculations ------------------------------------------------------------
weekly_results <- list()
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
  
  # df_week_summary <- df_week_summary |> filter(!is.na(RSJ_week))
  df_week_summary$portfolio <- assign_portfolio(df_week_summary, "RSJ_week", n_portfolios = 5)
  weekly_results[[week_id]] <- df_week_summary
  
  # Berekeningen dropped
  dropped_file <- file.path("data/setdifference", paste0("dropped_data_", clean_week_id, ".rds"))
  if (file.exists(dropped_file)) {
    dropped_df <- readRDS(dropped_file)
    
    if (!"returns_week" %in% colnames(dropped_df)) {
      dropped_df <- calculate_RSJ_day(dropped_df)
      dropped_df <- summarise_week(dropped_df, clean_week_id)
    }
    dropped_results[[clean_week_id]] <- dropped_df
  }
}


# Portfolio Sorting -------------------------------------------------------
portfolio_performance <- list()
weekly_spreads <- list()
all_joined <- list()

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
  
  # >>> FILTER OUT NA values in next_week_return <<<
  joined <- joined[!is.na(joined$next_week_return), ]
  joined$week_id <- week_ids[i]
  all_joined[[week_ids[i]]] <- joined
  
  
  
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

# Hier nog ff goed naar kijken
combined_df <- dplyr::bind_rows(all_joined)

portfolio_summary <- combined_df %>%
  group_by(portfolio) %>%
  summarise(
    total_log = sum(next_week_return, na.rm = TRUE),
    n = n(),
    avg_log = total_log / n,
    .groups = "drop"
  )





# FFC4 --------------------------------------------------------------------
portfolio_returns_weekly <- bind_rows(all_joined) %>%
  group_by(week_id, portfolio) %>%
  summarise(
    avg_log_return = mean(next_week_return, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(clean_week_id = str_remove(week_id, "^filtered_"))

ffc4_factors <- readRDS("data/metrics/FFC4.rds") %>%
  mutate(key = as.character(key))

returns_with_factors <- portfolio_returns_weekly %>%
  left_join(ffc4_factors, by = c("clean_week_id" = "key"))

library(sandwich)
library(broom)

ffc4_alpha_results <- returns_with_factors %>%
  group_by(portfolio) %>%
  group_modify(~{
    model <- lm(avg_log_return ~ mkt_excess + smb + hml + mom, data = .x)
    tidy(model, conf.int = TRUE, conf.level = 0.95) %>%
      filter(term == "(Intercept)") %>%
      select(estimate, std.error, statistic)
  })

