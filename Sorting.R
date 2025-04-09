# ============================================================================
# Portfolio Sorting for RSJ and RES Metrics (Equal-Weighted and Value-Weighted)
# Based on: https://www.tidy-finance.org/r/univariate-portfolio-sorts.html
# ============================================================================

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(RSQLite)
library(scales)
library(lmtest)
library(broom)
library(sandwich)

# Load Data ---------------------------------------------------------------
file_paths <- list.files("data/subset", pattern = "^filtered_\\d{4}-W\\d{2}\\.rds$", full.names = TRUE)

# Load FFC4 factors
ffc4_factors <- readRDS("data/metrics/FFC4.rds") %>%
  mutate(key = as.character(key))

# Assumes weekly_results is a list of weekly data.frames
weekly_all <- bind_rows(weekly_results) %>% 
  mutate(
    week = as.character(week),
    # returns_week = exp(returns_week / 100) - 1 # Convert log to arithmetic
  ) %>%
  select(week, permno, RSJ_week, RES_week, market_cap, returns_week)


# Portfolio Assignment Function -------------------------------------------
assign_portfolio <- function(data, sorting_variable, n_portfolios) {
  breakpoints <- data %>%
    pull({{ sorting_variable }}) %>%
    quantile(
      probs = seq(0, 1, length.out = n_portfolios + 1),
      na.rm = TRUE, names = FALSE
    )
  
  data %>%
    mutate(portfolio = findInterval(
      pull(pick(everything()), {{ sorting_variable }}),
      breakpoints,
      all.inside = TRUE
    )) %>%
    pull(portfolio)
}

# -------------------------------------------------------------------------
# Equal-Weighted Portfolio Sorts
# -------------------------------------------------------------------------

# RSJ Sort (Equal-Weighted)
rsj_portfolios_ew <- weekly_all %>%
  group_by(week) %>%
  mutate(
    RSJ_portfolio = assign_portfolio(pick(everything()), RSJ_week, n_portfolios = 5)
  ) %>%
  ungroup() %>%
  group_by(RSJ_portfolio, week) %>%
  summarise(
    ret_excess_equal = mean(returns_week, na.rm = FALSE),
    .groups = "drop"
  ) %>%
  rename(portfolio = RSJ_portfolio)

# RES Sort (Equal-Weighted)
res_portfolios_ew <- weekly_all %>%
  group_by(week) %>%
  mutate(
    RES_portfolio = assign_portfolio(pick(everything()), RES_week, n_portfolios = 5)
  ) %>%
  ungroup() %>%
  group_by(week, RES_portfolio) %>%
  summarise(
    ret_excess_equal = mean(next_week_return, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(portfolio = RES_portfolio)

# -------------------------------------------------------------------------
# Value-Weighted Portfolio Sorts
# -------------------------------------------------------------------------

# RSJ Sort (Value-Weighted)
rsj_portfolios_vw <- weekly_all %>%
  group_by(week) %>%
  mutate(
    RSJ_portfolio = assign_portfolio(pick(everything()), RSJ_week, n_portfolios = 5)
  ) %>%
  ungroup() %>%
  group_by(week, RSJ_portfolio) %>%
  summarise(
    ret_excess_weighted = weighted.mean(next_week_return, w = market_cap, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(portfolio = RSJ_portfolio)

# RES Sort (Value-Weighted)
res_portfolios_vw <- weekly_all %>%
  group_by(week) %>%
  mutate(
    RES_portfolio = assign_portfolio(pick(everything()), RES_week, n_portfolios = 5)
  ) %>%
  ungroup() %>%
  group_by(week, RES_portfolio) %>%
  summarise(
    ret_excess_weighted = weighted.mean(next_week_return, w = market_cap, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(portfolio = RES_portfolio)

# ============================================================================
# Result Summaries
# ============================================================================

# Equal-Weighted Portfolio Average Returns (RSJ)
rsj_avg_returns_ew <- rsj_portfolios_ew %>%
  group_by(portfolio) %>%
  summarize(
    avg_return = mean(ret_excess_equal, na.rm = TRUE),
    n_weeks = n(),
    .groups = "drop"
  )

# Equal-Weighted Portfolio Average Returns (RES)
res_avg_returns_ew <- res_portfolios_ew %>%
  group_by(portfolio) %>%
  summarize(
    avg_return = mean(ret_excess_equal, na.rm = TRUE),
    n_weeks = n(),
    .groups = "drop"
  )

# Value-Weighted Portfolio Average Returns (RSJ)
rsj_avg_returns_vw <- rsj_portfolios_vw %>%
  group_by(portfolio) %>%
  summarize(
    avg_return = mean(ret_excess_weighted, na.rm = TRUE),
    n_weeks = n(),
    .groups = "drop"
  )

# Value-Weighted Portfolio Average Returns (RES)
res_avg_returns_vw <- res_portfolios_vw %>%
  group_by(portfolio) %>%
  summarize(
    avg_return = mean(ret_excess_weighted, na.rm = TRUE),
    n_weeks = n(),
    .groups = "drop"
  )


# -------------------------------------------------------------------------
# Spread Calculation Function (Re-usable)
# -------------------------------------------------------------------------
compute_spread <- function(df, return_col) {
  df %>%
    pivot_wider(names_from = portfolio, values_from = !!sym(return_col), names_prefix = "p") %>%
    mutate(spread = p5 - p1) %>%
    select(week, spread)
}

# Compute Spreads
rsj_spread_ew <- compute_spread(rsj_portfolios_ew, "ret_excess_equal")
res_spread_ew <- compute_spread(res_portfolios_ew, "ret_excess_equal")

rsj_spread_vw <- compute_spread(rsj_portfolios_vw, "ret_excess_weighted")
res_spread_vw <- compute_spread(res_portfolios_vw, "ret_excess_weighted")

# -------------------------------------------------------------------------
# Newey-West T-statistics
# -------------------------------------------------------------------------
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

# -------------------------------------------------------------------------
# FFC4 Regressions on Equal-Weighted RSJ Portfolios (Optional)
# -------------------------------------------------------------------------
returns_with_factors_equal <- rsj_portfolios_ew %>%
  left_join(ffc4_factors, by = c("week" = "key"))

ffc4_alpha_results_equal <- returns_with_factors_equal %>%
  group_by(portfolio) %>%
  group_modify(~{
    model <- lm(ret_excess_equal ~ mkt_excess + smb + hml + mom, data = .x)
    tidy(model, conf.int = TRUE, conf.level = 0.95) %>%
      filter(term == "(Intercept)") %>%
      select(estimate, std.error, statistic)
  })