# Double sorting, heb ff ander bestandje want werd chaos in mn hoofd

# Libraries -----------------------------------------------------------------
library(dplyr)
library(stringr)
library(lmtest)
library(sandwich)
library(tidyverse)



# Data import -------------------------------------------------------------
ffc4_factors <- readRDS("data/metrics/FFC4.rds") %>%
  mutate(key = as.character(key))

# Creates the weekly all dataframe: One dataframe with all results  
weekly_all <- bind_rows(results) %>% 
  mutate(
    week = as.character(week),
  ) %>%
  select(week, permno, RSJ_week, RES_week, market_cap, next_week_return)

# Join the weekly_all dataframe with ffc4_factors to get the risk-free rate (rf)
weekly_all <- weekly_all %>%
  left_join(ffc4_factors, by = c("week" = "key")) %>%
  mutate(
    # Calculate the weekly risk-free rate based on the number of trading days in the week
    weekly_risk_free = trading_days_in_week * risk_free
  ) %>%
  ungroup() %>%
  select(week, permno, RSJ_week, RES_week, market_cap, next_week_return, weekly_risk_free)



# Code --------------------------------------------------------------------
# RSJ → RES Double Sorting:
rsj_res_portfolios_ew <- weekly_all %>%
  group_by(week) %>%
  mutate(
    rsj_bucket = assign_portfolio(pick(everything()), sorting_variable = "RSJ_week", n_portfolios = 5)
  ) %>%
  group_by(week, rsj_bucket) %>%
  mutate(
    res_bucket = assign_portfolio(pick(everything()), sorting_variable = "RES_week", n_portfolios = 5)
  ) %>%
  group_by(week, rsj_bucket, res_bucket) %>%
  summarise(
    avg_next_ret = mean(next_week_return, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(double_sort_type = "RSJ -> RES")

rsj_res_portfolios_vw <- weekly_all %>%
  group_by(week) %>%
  mutate(
    rsj_bucket = assign_portfolio(pick(everything()), sorting_variable = "RSJ_week", n_portfolios = 5)
  ) %>%
  group_by(week, rsj_bucket) %>%
  mutate(
    res_bucket = assign_portfolio(pick(everything()), sorting_variable = "RES_week", n_portfolios = 5)
  ) %>%
  group_by(week, rsj_bucket, res_bucket) %>%
  filter(!is.na(market_cap)) %>%
  summarise(
    avg_next_ret = weighted.mean(next_week_return, w = market_cap, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(double_sort_type = "RSJ -> RES")

# RES → RSJ Double Sorting:
res_rsj_portfolios_ew <- weekly_all %>%
  group_by(week) %>%
  mutate(
    res_bucket = assign_portfolio(pick(everything()), sorting_variable = "RES_week", n_portfolios = 5)
  ) %>%
  group_by(week, res_bucket) %>%
  mutate(
    rsj_bucket = assign_portfolio(pick(everything()), sorting_variable = "RSJ_week", n_portfolios = 5)
  ) %>%
  group_by(week, res_bucket, rsj_bucket) %>%
  summarise(
    avg_next_ret = mean(next_week_return, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(double_sort_type = "RES -> RSJ")

res_rsj_portfolios_vw <- weekly_all %>%
  group_by(week) %>%
  mutate(
    res_bucket = assign_portfolio(pick(everything()), sorting_variable = "RES_week", n_portfolios = 5)
  ) %>%
  group_by(week, res_bucket) %>%
  mutate(
    rsj_bucket = assign_portfolio(pick(everything()), sorting_variable = "RSJ_week", n_portfolios = 5)
  ) %>%
  group_by(week, res_bucket, rsj_bucket) %>%
  filter(!is.na(market_cap)) %>%
  summarise(
    avg_next_ret = weighted.mean(next_week_return, w = market_cap, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(double_sort_type = "RES -> RSJ")


# Portfolio return calculation  -------------------------------------------

# RSJ -> RES equal weighted
RSJ_RES_ar_ew <- rsj_res_portfolios_ew %>% 
  group_by (rsj_bucket, res_bucket) %>%
  summarize(
    avg_return = 10000 * mean(avg_next_ret, na.rm = TRUE),
    n(),
    .groups = "drop"
  )

# RSJ -> RES value weighted 
RSJ_RES_ar_vw <- rsj_res_portfolios_vw %>%
  group_by (rsj_bucket, res_bucket) %>%
  summarize (
    avg_return = 10000 * mean(avg_next_ret, na.rm = TRUE),
    n(),
    .groups = "drop"
  )

# RES -> RSJ equal weighted 
RES_RSJ_ar_ew <- res_rsj_portfolios_ew %>%
  group_by(res_bucket, rsj_bucket) %>%
  summarize (
    avg_return = 10000 * mean(avg_next_ret, na.rm = TRUE),
    n(), 
    .groups = "drop"
  )

# RES -> RSJ value weighted 
RES_RSJ_ar_vw <- res_rsj_portfolios_vw %>%
  group_by(res_bucket, rsj_bucket) %>%
  summarize (
    avg_return = 10000 * mean(avg_next_ret, na.rm = TRUE),
    n(),
    .groups = "drop"
  )

# Spread Calculation -----------------------------------------------

compute_double_sort_spread <- function(df, primary_bucket, secondary_bucket, return_col) {
  df %>%
    pivot_wider(names_from = {{ primary_bucket }}, values_from = {{ return_col }}, names_prefix = "p") %>%
    mutate(spread = 10000 * (p5 - p1)) %>%
    select(week, {{ secondary_bucket }}, spread)
}

# RSJ → RES spreads (within RES buckets)
spreads_rsj_res_ew <- compute_double_sort_spread(rsj_res_portfolios_ew, rsj_bucket, res_bucket, avg_next_ret)
spreads_rsj_res_vw <- compute_double_sort_spread(rsj_res_portfolios_vw, rsj_bucket, res_bucket, avg_next_ret)

# RES → RSJ spreads (within RSJ buckets)
spreads_res_rsj_ew <- compute_double_sort_spread(res_rsj_portfolios_ew, res_bucket, rsj_bucket, avg_next_ret)
spreads_res_rsj_vw <- compute_double_sort_spread(res_rsj_portfolios_vw, res_bucket, rsj_bucket, avg_next_ret)

nw_spread_tstat_by_bucket <- function(df, bucket_col) {
  df %>%
    group_by({{ bucket_col }}) %>%
    group_modify(~ {
      model <- lm(spread ~ 1, data = .x)
      tval <- coeftest(model, vcov. = NeweyWest(model))["(Intercept)", "t value"]
      tibble(t_stat = tval)
    }) %>%
    ungroup()
}

# Newey West t-statistics 
tstats_rsj_res_ew <- nw_spread_tstat_by_bucket(spreads_rsj_res_ew, res_bucket)
tstats_rsj_res_vw <- nw_spread_tstat_by_bucket(spreads_rsj_res_vw, res_bucket)
tstats_res_rsj_ew <- nw_spread_tstat_by_bucket(spreads_res_rsj_ew, rsj_bucket)
tstats_res_rsj_vw <- nw_spread_tstat_by_bucket(spreads_res_rsj_vw, rsj_bucket)


# Alpha calculation for the spreads  --------------------------------------

# Previously defined function in ExtraSorting
nw_tstat_FFC4 <- function(spreads_with_factors) {
  model <- lm(spread ~ mkt_excess + smb + hml + mom, data = spreads_with_factors)
  t_stat_intercept <- coeftest(model, vcov = NeweyWest(model))["(Intercept)", "t value"]
  return(t_stat_intercept)
}

# RSJ → RES
grouped_rsj_res_ew <- spreads_rsj_res_ew %>%
  left_join(ffc4_factors, by = c("week" = "key")) %>%
  group_by(res_bucket)

grouped_rsj_res_vw <- spreads_rsj_res_vw %>%
  left_join(ffc4_factors, by = c("week" = "key")) %>%
  group_by(res_bucket)

# RES → RSJ
grouped_res_rsj_ew <- spreads_res_rsj_ew %>%
  left_join(ffc4_factors, by = c("week" = "key")) %>%
  group_by(rsj_bucket)

grouped_res_rsj_vw <- spreads_res_rsj_vw %>%
  left_join(ffc4_factors, by = c("week" = "key")) %>%
  group_by(rsj_bucket)

nw_alpha_by_bucket <- function(df, bucket_col) {
  df %>%
    group_by({{ bucket_col }}) %>%
    group_modify(~ {
      model <- lm(spread ~ mkt_excess + smb + hml + mom, data = .x)
      coef_summary <- coeftest(model, vcov = NeweyWest(model))
      tibble(
        alpha = coef_summary["(Intercept)", "Estimate"],
        t_stat_alpha = coef_summary["(Intercept)", "t value"]
      )
    }) %>%
    ungroup()
}

# RSJ → RES regressions (on RES buckets)
alpha_tstats_rsj_res_ew <- nw_alpha_by_bucket(grouped_rsj_res_ew, res_bucket)
alpha_tstats_rsj_res_vw <- nw_alpha_by_bucket(grouped_rsj_res_vw, res_bucket)

# RES → RSJ regressions (on RSJ buckets)
alpha_tstats_res_rsj_ew <- nw_alpha_by_bucket(grouped_res_rsj_ew, rsj_bucket)
alpha_tstats_res_rsj_vw <- nw_alpha_by_bucket(grouped_res_rsj_vw, rsj_bucket)


# # For RSJ → RES sort:
# spreads_by_rsj_bucket_ew <- rsj_res_portfolios_ew %>%
#   group_by(week, res_bucket) %>%
#   summarise(
#     high = max(avg_next_ret, na.rm = TRUE),
#     low = min(avg_next_ret, na.rm = TRUE),
#     spread = high - low,
#     .groups = "drop"
#   )
# 
# spreads_by_rsj_bucket_vw <- rsj_res_portfolios_vw %>%
#   group_by(week, res_bucket) %>%
#   summarise(
#     high = max(avg_next_ret, na.rm = TRUE),
#     low = min(avg_next_ret, na.rm = TRUE),
#     spread = high - low,
#     .groups = "drop"
#   )
# 
# # For RES → RSJ sort:
# spreads_by_res_bucket_ew <- res_rsj_portfolios_ew %>%
#   group_by(week, rsj_bucket) %>%
#   summarise(
#     high = max(avg_next_ret, na.rm = TRUE),
#     low = min(avg_next_ret, na.rm = TRUE),
#     spread = high - low,
#     .groups = "drop"
#   )
# 
# spreads_by_res_bucket_vw <- res_rsj_portfolios_vw %>%
#   group_by(week, rsj_bucket) %>%
#   summarise(
#     high = max(avg_next_ret, na.rm = TRUE),
#     low = min(avg_next_ret, na.rm = TRUE),
#     spread = high - low,
#     .groups = "drop"
#   )
# 
# # Regression for RSJ → RES Spread:
# model_rsj_ew <- lm(spread ~ 1, data = spreads_by_rsj_bucket_ew)
# nw_rsj_ew <- coeftest(model_rsj_ew, vcov = NeweyWest)
# 
# model_rsj_vw <- lm(spread ~ 1, data = spreads_by_rsj_bucket_vw)
# nw_rsj_vw <- coeftest(model_rsj_vw, vcov = NeweyWest)
# 
# # Regression for RES → RSJ Spread:
# model_res_ew <- lm(spread ~ 1, data = spreads_by_res_bucket_ew)
# nw_res_ew <- coeftest(model_res_ew, vcov = NeweyWest)
# 
# model_res_vw <- lm(spread ~ 1, data = spreads_by_res_bucket_vw)
# nw_res_vw <- coeftest(model_res_vw, vcov = NeweyWest)
# 
# # Print results:
# print(nw_rsj_ew)
# print(nw_rsj_vw)
# print(nw_res_ew)
# print(nw_res_vw)


# Hier nog ff dan FFC4 toevoegen maar zou dat gewoon met chat doen

