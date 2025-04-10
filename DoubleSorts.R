# Double sorting, heb ff ander bestandje want werd chaos in mn hoofd


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

# Libraries -----------------------------------------------------------------
library(dplyr)
library(stringr)
library(lmtest)
library(sandwich)


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


# Spread Calculation -----------------------------------------------
# For RSJ → RES sort:
spreads_by_rsj_bucket_ew <- rsj_res_portfolios_ew %>%
  group_by(week, rsj_bucket) %>%
  summarise(
    high = max(avg_next_ret, na.rm = TRUE),
    low = min(avg_next_ret, na.rm = TRUE),
    spread = high - low,
    .groups = "drop"
  )

spreads_by_rsj_bucket_vw <- rsj_res_portfolios_vw %>%
  group_by(week, rsj_bucket) %>%
  summarise(
    high = max(avg_next_ret, na.rm = TRUE),
    low = min(avg_next_ret, na.rm = TRUE),
    spread = high - low,
    .groups = "drop"
  )

# For RES → RSJ sort:
spreads_by_res_bucket_ew <- res_rsj_portfolios_ew %>%
  group_by(week, res_bucket) %>%
  summarise(
    high = max(avg_next_ret, na.rm = TRUE),
    low = min(avg_next_ret, na.rm = TRUE),
    spread = high - low,
    .groups = "drop"
  )

spreads_by_res_bucket_vw <- res_rsj_portfolios_vw %>%
  group_by(week, res_bucket) %>%
  summarise(
    high = max(avg_next_ret, na.rm = TRUE),
    low = min(avg_next_ret, na.rm = TRUE),
    spread = high - low,
    .groups = "drop"
  )

# Regression for RSJ → RES Spread:
model_rsj_ew <- lm(spread ~ 1, data = spreads_by_rsj_bucket_ew)
nw_rsj_ew <- coeftest(model_rsj_ew, vcov = NeweyWest)

model_rsj_vw <- lm(spread ~ 1, data = spreads_by_rsj_bucket_vw)
nw_rsj_vw <- coeftest(model_rsj_vw, vcov = NeweyWest)

# Regression for RES → RSJ Spread:
model_res_ew <- lm(spread ~ 1, data = spreads_by_res_bucket_ew)
nw_res_ew <- coeftest(model_res_ew, vcov = NeweyWest)

model_res_vw <- lm(spread ~ 1, data = spreads_by_res_bucket_vw)
nw_res_vw <- coeftest(model_res_vw, vcov = NeweyWest)

# Print results:
print(nw_rsj_ew)
print(nw_rsj_vw)
print(nw_res_ew)
print(nw_res_vw)


# Hier nog ff dan FFC4 toevoegen maar zou dat gewoon met chat doen

