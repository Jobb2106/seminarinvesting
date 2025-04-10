# Libraries ---------------------------------------------------------------
library(tidyverse)
library(RSQLite)
library(scales)
library(lmtest)
library(broom)
library(sandwich)

# Load FFC4 factors
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

# Function to assign portfolios 
assign_portfolio <- function(data, 
                             sorting_variable, 
                             n_portfolios) {
  # Compute breakpoints
  breakpoints <- data |>
    pull({{ sorting_variable }}) |>
    quantile(
      probs = seq(0, 1, length.out = n_portfolios + 1),
      na.rm = TRUE,
      names = FALSE
    )
  
  # Assign portfolios
  assigned_portfolios <- data |>
    mutate(portfolio = findInterval(
      pick(everything()) |>
        pull({{ sorting_variable }}),
      breakpoints,
      all.inside = TRUE
    )) |>
    pull(portfolio)
  
  # Output
  return(assigned_portfolios)
}


# RSJ portfolio Equal Weighted -------------------------------------------

RSJ_portfolios_ew <- weekly_all |>
  group_by(week) |>
  mutate(
    portfolio = assign_portfolio(
      data = pick(everything()),
      sorting_variable = RSJ_week,
      n_portfolios = 5
    ),
    portfolio = as.factor(portfolio)
  ) |>
  group_by(portfolio, week) |>
  summarize(
    ret_excess_rsj_ew = mean(next_week_return),
    .groups = "drop"
  )

# Equal-Weighted Portfolio Average Returns (RSJ)
rsj_ar_ew <- RSJ_portfolios_ew %>%
  group_by(portfolio) %>%
  summarize(
    avg_return = 10000 * mean(ret_excess_rsj_ew, na.rm = TRUE),
    n_weeks = n(),
    .groups = "drop"
  )


# RSJ portfolio Value Weighted  -------------------------------------------

RSJ_portfolios_vw <- weekly_all |>
  group_by(week) |>
  mutate(
    portfolio = assign_portfolio(
      data = pick(everything()),
      sorting_variable = RSJ_week,
      n_portfolios = 5
    ),
    portfolio = as.factor(portfolio)
  ) |>
  filter(!is.na(market_cap)) |>  # Exclude firms with NA in market_cap
  group_by(portfolio, week) |>
  summarize(
    ret_excess_rsj_vw = weighted.mean(next_week_return, w = market_cap, na.rm = TRUE),
    .groups = "drop"
  )

# Value-weighted Portfolio Average Returns (RSJ)
rsj_ar_vw <- RSJ_portfolios_vw %>%
  group_by(portfolio) %>%
  summarize(
    avg_return = 10000 * mean(ret_excess_rsj_vw, na.rm = TRUE),
    n_weeks = n(),
    .groups = "drop"
  )


# RES portfolio Equal Weighted --------------------------------------------

RES_portfolios_ew <- weekly_all |>
  group_by(week) |>
  mutate(
    portfolio = assign_portfolio(
      data = pick(everything()),
      sorting_variable = RES_week,
      n_portfolios = 5
    ),
    portfolio = as.factor(portfolio)
  ) |>
  group_by(portfolio, week) |>
  summarize(
    ret_excess_res_ew = mean(next_week_return),
    .groups = "drop"
  )

# Equal-Weighted Portfolio Average Returns (RES)
res_ar_ew <- RES_portfolios_ew %>%
  group_by(portfolio) %>%
  summarize(
    avg_return = 10000 * mean(ret_excess_res_ew, na.rm = TRUE),
    n_weeks = n(),
    .groups = "drop"
  )

# RES portfolio value-weighted --------------------------------------------

RES_portfolios_vw <- weekly_all |>
  group_by(week) |>
  mutate(
    portfolio = assign_portfolio(
      data = pick(everything()),
      sorting_variable = RES_week,
      n_portfolios = 5
    ),
    portfolio = as.factor(portfolio)
  ) |>
  filter(!is.na(market_cap)) |>  # Exclude firms with NA in market_cap
  group_by(portfolio, week) |>
  summarize(
    ret_excess_res_vw = weighted.mean(next_week_return, w = market_cap, na.rm = TRUE),
    .groups = "drop"
  )

# Value-Weighted Portfolio Average Returns (RES)
res_ar_vw <- RES_portfolios_vw %>%
  group_by(portfolio) %>%
  summarize(
    avg_return = 10000 * mean(ret_excess_res_vw, na.rm = TRUE),
    n_weeks = n(),
    .groups = "drop"
  )

compute_spread <- function(df, return_col) {
  df %>%
    pivot_wider(names_from = portfolio, values_from = !!sym(return_col), names_prefix = "p") %>%
    mutate(spread = 10000 * (p5 - p1)) %>%
    select(week, spread)
}

# Compute Spreads
rsj_spread_ew <- compute_spread(RSJ_portfolios_ew, "ret_excess_rsj_ew")
res_spread_ew <- compute_spread(RES_portfolios_ew, "ret_excess_res_ew")

rsj_spread_vw <- compute_spread(RSJ_portfolios_vw, "ret_excess_rsj_vw")
res_spread_vw <- compute_spread(RES_portfolios_vw, "ret_excess_res_vw")

# Newey west for spreads
nw_tstat <- function(spread_ts) {
  model <- lm(spread ~ 1, data = spread_ts)
  t_value <- coeftest(model, vcov. = NeweyWest(model))[1, "t value"]
  return(t_value)
}

# T-stats
rsj_tstat_ew <- nw_tstat(rsj_spread_ew)
res_tstat_ew <- nw_tstat(res_spread_ew)
rsj_tstat_vw <- nw_tstat(rsj_spread_vw)
res_tstat_vw <- nw_tstat(res_spread_vw)

# FFC4 for RSJ Equal Weighted Portfolio  ----------------------------------

RSJ_returns_with_factors_equal <- RSJ_portfolios_ew %>%
  left_join(ffc4_factors, by = c("week" = "key")) %>%
  mutate(
    # Calculate weekly risk-free rate here after the join
    weekly_risk_free = trading_days_in_week * risk_free,
    # Adjust for risk-free rate for the regression
    ret_excess_rsj_ew = ret_excess_rsj_ew - weekly_risk_free
  )

ffc4_alpha_rsj_ew <- RSJ_returns_with_factors_equal %>%
  group_by(portfolio) %>%
  group_modify(~{
    model <- lm(ret_excess_rsj_ew ~ mkt_excess + smb + hml + mom, data = .x)
    tidy(model, conf.int = TRUE, conf.level = 0.95) %>%
      filter(term == "(Intercept)") %>%
      select(estimate, std.error, statistic)
  })
ffc4_alpha_rsj_ew <- 10000 * ffc4_alpha_rsj_ew

# FFC4 for RSJ Value Weighted Portfolio  ----------------------------------

RSJ_returns_with_factors_value <- RSJ_portfolios_vw %>%
  left_join(ffc4_factors, by = c("week" = "key")) %>%
  mutate(
    # Calculate weekly risk-free rate here after the join
    weekly_risk_free = trading_days_in_week * risk_free,
    # Adjust for risk-free rate for the regression
    ret_excess_rsj_vw = ret_excess_rsj_vw - weekly_risk_free
  )

ffc4_alpha_rsj_vw <- RSJ_returns_with_factors_value %>%
  group_by(portfolio) %>%
  group_modify(~{
    model <- lm(ret_excess_rsj_vw ~ mkt_excess + smb + hml + mom, data = .x)
    tidy(model, conf.int = TRUE, conf.level = 0.95) %>%
      filter(term == "(Intercept)") %>%
      select(estimate, std.error, statistic)
  })
ffc4_alpha_rsj_vw <- 10000 * ffc4_alpha_rsj_vw

# FFC4 for RES Equal Weighted Portfolio  ----------------------------------

RES_returns_with_factors_equal <- RES_portfolios_ew %>%
  left_join(ffc4_factors, by = c("week" = "key")) %>%
  mutate(
    # Calculate weekly risk-free rate here after the join
    weekly_risk_free = trading_days_in_week * risk_free,
    # Adjust for risk-free rate for the regression
    ret_excess_res_ew = ret_excess_res_ew - weekly_risk_free
  )

ffc4_alpha_res_ew <- RES_returns_with_factors_equal %>%
  group_by(portfolio) %>%
  group_modify(~{
    model <- lm(ret_excess_res_ew ~ mkt_excess + smb + hml + mom, data = .x)
    tidy(model, conf.int = TRUE, conf.level = 0.95) %>%
      filter(term == "(Intercept)") %>%
      select(estimate, std.error, statistic)
  })
ffc4_alpha_res_ew <- 10000 * ffc4_alpha_res_ew

# FFC4 for RES Value Weighted Portfolio  ----------------------------------

RES_returns_with_factors_value <- RES_portfolios_vw %>%
  left_join(ffc4_factors, by = c("week" = "key")) %>%
  mutate(
    # Calculate weekly risk-free rate here after the join
    weekly_risk_free = trading_days_in_week * risk_free,
    # Adjust for risk-free rate for the regression
    ret_excess_res_vw = ret_excess_res_vw - weekly_risk_free
  )

ffc4_alpha_res_vw <- RES_returns_with_factors_value %>%
  group_by(portfolio) %>%
  group_modify(~{
    model <- lm(ret_excess_res_vw ~ mkt_excess + smb + hml + mom, data = .x)
    tidy(model, conf.int = TRUE, conf.level = 0.95) %>%
      filter(term == "(Intercept)") %>%
      select(estimate, std.error, statistic)
  })
ffc4_alpha_res_vw <- 10000 * ffc4_alpha_res_vw
 





