# This script is used to calculate the portfolio sorts for the jump and continuous portfolios based on RES 
# This script computes equal- and value-weighted portfolio returns 
# sorted on RES, for both continuous and jump components.
# It also calculates spreads, t-stats (Newey-West), and FFC4 alphas.

# Libraries 
library(tidyverse)
library(RSQLite)
library(scales)
library(lmtest)
library(broom)
library(sandwich)

# Load FFC4 factors
ffc4_factors <- readRDS("input/FFC4.rds") %>%
  mutate(key = as.character(key))

# Creates the weekly all dataframe: One dataframe with all results  
weekly_all <- bind_rows(results) %>% 
  mutate(week = as.character(week)) %>%
  select(week, permno, RES_week, market_cap, next_week_return, AJR_portfolio)

# Join the weekly_all dataframe with ffc4_factors to get the risk-free rate (rf)
weekly_all <- weekly_all %>%
  left_join(ffc4_factors, by = c("week" = "key")) %>%
  mutate(weekly_risk_free = trading_days_in_week * risk_free) %>%
  ungroup() %>%
  select(week, permno, RES_week, market_cap, next_week_return, weekly_risk_free, AJR_portfolio)

# For robustness analysis, create a subset of 10 years. Uncomment relevant one when performing robustness check

# Subset for 1993–2002
# weekly_all <- weekly_all[as.integer(substr(weekly_all$week, 1, 4)) %in% 1993:2002, ]

# Subset for 2003–2012
weekly_all <- weekly_all[as.integer(substr(weekly_all$week, 1, 4)) %in% 2003:2012, ]

# Subset for 2013–2023
# weekly_all <- weekly_all[as.integer(substr(weekly_all$week, 1, 4)) %in% 2013:2023, ]

# split the weekly_all dataframe into a continuous and jump dataframe  
continuous_df <- weekly_all %>% filter(AJR_portfolio == "continuous")
jump_df <- weekly_all %>% filter(AJR_portfolio == "jump")

# Function to assign portfolios 
assign_portfolio <- function(data, sorting_variable, n_portfolios) {
  breakpoints <- data |>
    pull({{ sorting_variable }}) |>
    quantile(probs = seq(0, 1, length.out = n_portfolios + 1), na.rm = TRUE, names = FALSE)
  
  assigned_portfolios <- data |>
    mutate(portfolio = findInterval(
      pick(everything()) |> pull({{ sorting_variable }}),
      breakpoints,
      all.inside = TRUE
    )) |>
    pull(portfolio)
  
  return(assigned_portfolios)
}

# RES continuous sorted portfolio equal weighted ------------------
RES_cont_portfolios_ew <- continuous_df %>%
  group_by(week) %>%
  mutate(
    portfolio = assign_portfolio(data = pick(everything()), sorting_variable = RES_week, n_portfolios = 5),
    portfolio = as.factor(portfolio)
  ) %>%
  group_by(portfolio, week) %>%
  summarize(ret_excess = mean(next_week_return), .groups = "drop")

res_cont_ar_ew <- RES_cont_portfolios_ew %>%
  group_by(portfolio) %>%
  summarize(avg_return = 10000 * mean(ret_excess, na.rm = TRUE), n_weeks = n(), .groups = "drop")

# RES continuous sorted portfolio value weighted ------------------
RES_cont_portfolios_vw <- continuous_df %>%
  group_by(week) %>%
  mutate(
    portfolio = assign_portfolio(data = pick(everything()), sorting_variable = RES_week, n_portfolios = 5), 
    portfolio = as.factor(portfolio)
  ) %>%
  filter(!is.na(market_cap)) %>%
  group_by(portfolio, week) %>%
  summarize(ret_excess = weighted.mean(next_week_return, w = market_cap, na.rm = TRUE), .groups = "drop")

res_cont_ar_vw <- RES_cont_portfolios_vw %>% 
  group_by(portfolio) %>%
  summarize(avg_return = 10000 * mean(ret_excess, na.rm = TRUE), n_weeks = n(), .groups = "drop")

# RES jump sorted portfolio equal weighted ------------------------
RES_jump_portfolios_ew <- jump_df %>%
  group_by(week) %>%
  mutate(
    portfolio = assign_portfolio(data = pick(everything()), sorting_variable = RES_week, n_portfolios = 5),
    portfolio = as.factor(portfolio)
  ) %>%
  group_by(portfolio, week) %>%
  summarize(ret_excess = mean(next_week_return), .groups = "drop")

res_jump_ar_ew <- RES_jump_portfolios_ew %>%
  group_by(portfolio) %>%
  summarize(avg_return = 10000 * mean(ret_excess, na.rm = TRUE), n_weeks = n(), .groups = "drop")

# RES jump sorted portfolio value weighted ------------------------
RES_jump_portfolios_vw <- jump_df %>%
  group_by(week) %>%
  mutate(
    portfolio = assign_portfolio(data = pick(everything()), sorting_variable = RES_week, n_portfolios = 5), 
    portfolio = as.factor(portfolio)
  ) %>%
  filter(!is.na(market_cap)) %>%
  group_by(portfolio, week) %>%
  summarize(ret_excess = weighted.mean(next_week_return, w = market_cap, na.rm = TRUE), .groups = "drop")

res_jump_ar_vw <- RES_jump_portfolios_vw %>% 
  group_by(portfolio) %>%
  summarize(avg_return = 10000 * mean(ret_excess, na.rm = TRUE), n_weeks = n(), .groups = "drop")

# Compute the spreads  ----------------------------------------------------
compute_spread <- function(df, return_col) {
  df %>%
    pivot_wider(names_from = portfolio, values_from = !!sym(return_col), names_prefix = "p") %>%
    mutate(spread = 10000 * (p5 - p1)) %>%
    select(week, spread)
}

res_cont_spread_ew <- compute_spread(RES_cont_portfolios_ew, "ret_excess") 
res_cont_spread_vw <- compute_spread(RES_cont_portfolios_vw, "ret_excess")
res_jump_spread_ew <- compute_spread(RES_jump_portfolios_ew, "ret_excess")
res_jump_spread_vw <- compute_spread(RES_jump_portfolios_vw, "ret_excess")

# Newey west for spreads --------------------------------------------------
nw_tstat <- function(spread_ts) {
  spread_ts <- spread_ts %>% filter(!is.na(spread))
  model <- lm(spread ~ 1, data = spread_ts)
  t_value <- coeftest(model, vcov. = NeweyWest(model))[1, "t value"]
  return(t_value)
}

res_cont_tstat_ew <- nw_tstat(res_cont_spread_ew)
res_cont_tstat_vw <- nw_tstat(res_cont_spread_vw)
res_jump_tstat_ew <- nw_tstat(res_jump_spread_ew)
res_jump_tstat_vw <- nw_tstat(res_jump_spread_vw)

# FFC4 Regressions for RES portfolios -------------------------------------

add_ffc4 <- function(df) {
  df %>%
    left_join(ffc4_factors, by = c("week" = "key")) %>%
    mutate(
      weekly_risk_free = trading_days_in_week * risk_free,
      ret_excess  = ret_excess - weekly_risk_free 
    )
}

ffc4_regression <- function(df) {
  df %>%
    group_by(portfolio) %>%
    group_modify(~{
      model <- lm(ret_excess ~ mkt_excess + smb + hml + mom, data = .x)
      tidy(model, conf.int = TRUE, conf.level = 0.95) %>%
        filter(term == "(Intercept)") %>%
        select(estimate, std.error, statistic)
    }) %>%
    mutate(across(everything(), ~ 10000 * .x))
}

ffc4_alpha_res_cont_ew <- RES_cont_portfolios_ew |> add_ffc4() |> ffc4_regression()
ffc4_alpha_res_cont_vw <- RES_cont_portfolios_vw |> add_ffc4() |> ffc4_regression()
ffc4_alpha_res_jump_ew <- RES_jump_portfolios_ew |> add_ffc4() |> ffc4_regression()
ffc4_alpha_res_jump_vw <- RES_jump_portfolios_vw |> add_ffc4() |> ffc4_regression()

# FFC4 regression on spreads ----------------------------------------------
nw_tstat_FFC4 <- function(spreads) {
  model <- lm(spread ~ mkt_excess + smb + hml + mom, data = spreads)
  t_stat_intercept <- coeftest(model, vcov = NeweyWest(model))["(Intercept)", "t value"]
  return(t_stat_intercept)
}

grouped_data_res_cont_ew <- res_cont_spread_ew %>% left_join(ffc4_factors, by = c("week" = "key")) %>% group_by(week)
grouped_data_res_cont_vw <- res_cont_spread_vw %>% left_join(ffc4_factors, by = c("week" = "key")) %>% group_by(week)
grouped_data_res_jump_ew <- res_jump_spread_ew %>% left_join(ffc4_factors, by = c("week" = "key")) %>% group_by(week)
grouped_data_res_jump_vw <- res_jump_spread_vw %>% left_join(ffc4_factors, by = c("week" = "key")) %>% group_by(week)

res_cont_alpha_tstat_ew <- nw_tstat_FFC4(grouped_data_res_cont_ew)
res_cont_alpha_tstat_vw <- nw_tstat_FFC4(grouped_data_res_cont_vw)
res_jump_alpha_tstat_ew <- nw_tstat_FFC4(grouped_data_res_jump_ew)
res_jump_alpha_tstat_vw <- nw_tstat_FFC4(grouped_data_res_jump_vw)



