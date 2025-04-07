# This script is for the programming of the decomposition of the RV

# Data --------------------------------------------------------------------
# Laad de data uit GitHub
file_paths <- list.files("data/subset", pattern = "^filtered_\\d{4}-W\\d{2}\\.rds$", full.names = TRUE)

# dropped paths
dropped_files <- list.files("data/setdifference", pattern = "^dropped_data_\\d{4}-W\\d{2}\\.rds$", full.names = TRUE)

# FFC4 factors
ffc4_factors <- readRDS("data/metrics/FFC4.rds") %>%
  mutate(key = as.character(key))

# Market Cap data
market_cap <- readRDS("data/metrics/MarketCap.rds")


# Libraries ---------------------------------------------------------------
library(dplyr)
library(stringr)
library(lmtest)
library(sandwich)
library(purrr)
library(broom)
library(data.table)

# RSJ/RES calculations -----------------------------------
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

calculate_RES_day <- function(df, p = 0.05, scaling_factor = 78^0.5) {
  df %>%
    rowwise() %>%  # Process each row individually
    mutate(
      ES_p = {
        # Unlist the 5-minute returns for this row
        r_5m <- unlist(returns_5m)
        # Compute the quantile at probability p
        q_p <- quantile(r_5m, probs = p, na.rm = TRUE)
        # Compute the mean of returns below or equal to the quantile
        mean(r_5m[r_5m <= q_p], na.rm = TRUE)
      },
      RES = scaling_factor * ES_p
    ) %>%
    ungroup()
}


summarise_week <- function(df, week_id) {
  df %>%
    group_by(permno) %>%
    summarise(
      RSJ_week = mean(RSJ_day, na.rm = TRUE),
      returns_week = sum(open_close_log_ret, na.rm = TRUE),
      RES_week = sqrt(n()) * sum(RES, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(week_id = week_id)
}

test <- readRDS(file_paths[1])
test <- calculate_RES_day(test)
test <- calculate_RSJ_day(test)
test <- summarise_week(test, "filtered_2013-W02")

# Calculations ------------------------------------------------------------
weekly_results <- list()
dropped_results <- list()

for (file in file_paths) {
  df <- readRDS(file)
  week_id <- str_remove(basename(file), "\\.rds$")
  clean_week_id <- str_remove(week_id, "^filtered_")
  
  # Normal calculations: first compute RSJ_day and RES
  df <- calculate_RSJ_day(df)
  df <- calculate_RES_day(df)  
  
  # Summarise weekly values (this aggregates RSJ_day, returns, and RES into weekly data)
  df_week_summary <- summarise_week(df, week_id)
  df_week_summary <- df_week_summary %>% filter(!is.na(RSJ_week))
  
  # Assign portfolios for both RSJ and RES sorts:
  df_week_summary$RSJ_portfolio <- df_week_summary %>%
    mutate(
      RSJ_portfolio = assign_portfolio(., sorting_variable = "RSJ_week", n_portfolios = 5),
      RES_portfolio = assign_portfolio(., sorting_variable = "RES_week", n_portfolios = 5)
    )
  
  weekly_results[[week_id]] <- df_week_summary
  
  # Process dropped data if exists
  if (file.exists(dropped_file)) {
    dropped_df <- readRDS(dropped_file)
    if (!"returns_week" %in% colnames(dropped_df)) {
      dropped_df <- calculate_RSJ_day(dropped_df)
      dropped_df <- calculate_RES_day(dropped_df)  # <-- add this call
      dropped_df <- summarise_week(dropped_df, clean_week_id)
    }
    dropped_results[[clean_week_id]] <- dropped_df
  }
}

# Portfolio Sorting -------------------------------------------------------
portfolio_performance <- list()
all_joined <- list()
week_ids <- sort(names(weekly_results))

for (i in 1:(length(week_ids) - 1)) {
  current_week_id <- week_ids[i]
  next_week_id <- week_ids[i + 1]
  clean_next_id <- sub("^filtered_", "", next_week_id)
  
  current_week <- weekly_results[[current_week_id]]
  next_week <- weekly_results[[next_week_id]]
  dropped_next <- dropped_results[[clean_next_id]]
  
  # Bereken next week returns
  joined <- add_next_week_return(
    current_week_df = current_week,
    next_week_df = next_week,
    dropped_df = dropped_next
  )
  
  # Filter rijen zonder next_week_return
  joined <- joined[!is.na(joined$next_week_return), ]
  
  # Maak een clean week datum door de prefix te verwijderen
  clean_week_date <- str_remove(current_week_id, "^filtered_")
  joined$week_id <- clean_week_date
  all_joined[[current_week_id]] <- joined
  
  # Rolling join met market_cap_df (gebruik hier het correcte dataframe)
  joined_dt <- as.data.table(joined)
  market_cap_dt <- as.data.table(market_cap)  # Gebruik market_cap_df
  
  # Converteer week_id naar Date met een expliciet formaat
  joined_dt[, week_id := as.Date(week_id, format = "%Y-%m-%d")]
  market_cap_dt[, week_id := as.Date(week_id, format = "%Y-%m-%d")]
  
  # Stel de keys in voor de join
  setkey(joined_dt, permno, week_id)
  setkey(market_cap_dt, permno, week_id)
  
  # Rolling join: als er geen exacte match is, wordt de laatste beschikbare market cap gebruikt.
  joined_dt <- market_cap_dt[joined_dt, roll = TRUE]
  joined <- as_tibble(joined_dt)
  
  # Bereken de equally weighted performance
  perf_eq <- summarise_portfolios(joined) %>%
    mutate(type = "equal", week_id = clean_week_date)
  
  # Bereken de value weighted performance
  perf_vw <- summarise_portfolios_value_weighted(joined) %>%
    mutate(type = "value", week_id = clean_week_date)
  
  # Combineer beide resultaten
  perf <- bind_rows(perf_eq, perf_vw)
  portfolio_performance[[current_week_id]] <- perf
}


# T5-T1, t value ----------------------------------------------------------
# Equally Weighted
performance_panel_equal <- bind_rows(portfolio_performance) %>%
  filter(type == "equal") %>%
  group_by(week_id) %>%
  summarise(spread = avg_return[portfolio == max(portfolio)] - avg_return[portfolio == min(portfolio)], 
            .groups = "drop")

model_equal <- lm(spread ~ 1, data = performance_panel_equal)
coeftest(model_equal, vcov = NeweyWest)

# Value Weighted:
performance_panel_value <- bind_rows(portfolio_performance) %>%
  filter(type == "value") %>%
  group_by(week_id) %>%
  summarise(spread = avg_return[portfolio == max(portfolio)] - avg_return[portfolio == min(portfolio)], 
            .groups = "drop")

model_value <- lm(spread ~ 1, data = performance_panel_value)
coeftest(model_value, vcov = NeweyWest)


# Portfolio Summary -------------------------------------------------------
# Eerst: bind_rows(all_joined) geeft alle observaties met next_week_return
combined_df <- bind_rows(portfolio_performance)

# Equally weighted portfolio summary:
portfolio_summary_equal <- combined_df %>%
  filter(type == "equal") %>%
  group_by(portfolio) %>%
  summarise(mean_return = mean(avg_return, na.rm = TRUE),
            .groups = "drop")

# Value weighted portfolio summary:
portfolio_summary_value <- combined_df %>%
  filter(type == "value") %>%
  group_by(portfolio) %>%
  summarise(mean_return = mean(avg_return, na.rm = TRUE),
            .groups = "drop")


# FFC4 --------------------------------------------------------------------
# We berekenen eerst de wekelijkse portefeuille returns per type.
# Voor de equally weighted variant:
portfolio_returns_weekly_equal <- combined_df %>%
  filter(type == "equal") %>%
  group_by(week_id, portfolio) %>%
  summarise(avg_log_return = mean(avg_return, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(clean_week_id = str_remove(week_id, "^filtered_"))

returns_with_factors_equal <- portfolio_returns_weekly_equal %>%
  left_join(ffc4_factors, by = c("clean_week_id" = "key"))

ffc4_alpha_results_equal <- returns_with_factors_equal %>%
  group_by(portfolio) %>%
  group_modify(~{
    model <- lm(avg_log_return ~ mkt_excess + smb + hml + mom, data = .x)
    tidy(model, conf.int = TRUE, conf.level = 0.95) %>%
      filter(term == "(Intercept)") %>%
      select(estimate, std.error, statistic)
  })

# Voor de value weighted variant:
portfolio_returns_weekly_value <- combined_df %>%
  filter(type == "value") %>%
  group_by(week_id, portfolio) %>%
  summarise(avg_log_return = mean(avg_return, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(clean_week_id = str_remove(week_id, "^filtered_"))

returns_with_factors_value <- portfolio_returns_weekly_value %>%
  left_join(ffc4_factors, by = c("clean_week_id" = "key"))

ffc4_alpha_results_value <- returns_with_factors_value %>%
  group_by(portfolio) %>%
  group_modify(~{
    model <- lm(avg_log_return ~ mkt_excess + smb + hml + mom, data = .x)
    tidy(model, conf.int = TRUE, conf.level = 0.95) %>%
      filter(term == "(Intercept)") %>%
      select(estimate, std.error, statistic)
  })

