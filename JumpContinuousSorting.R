# This script is used to calculate the portfolio sorts for the jump and continuous portfolios based on negative JR 

# Libraries 
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
  select(week, permno, jr_neg, market_cap, next_week_return, AJR_portfolio)

# Join the weekly_all dataframe with ffc4_factors to get the risk-free rate (rf)
weekly_all <- weekly_all %>%
  left_join(ffc4_factors, by = c("week" = "key")) %>%
  mutate(
    # Calculate the weekly risk-free rate based on the number of trading days in the week
    weekly_risk_free = trading_days_in_week * risk_free
  ) %>%
  ungroup() %>%
  select(week, permno, jr_neg, market_cap, next_week_return, weekly_risk_free, AJR_portfolio)

# split the weekly_all dataframe into a continuous and jump dataframe  
continuous_df <- weekly_all %>%
  filter(AJR_portfolio == "continuous")

jump_df <- weekly_all %>%
  filter(AJR_portfolio == "jump")

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


# JR negative continuous sorted portfolio equal weighted ------------------
JR_cont_portfolios_ew <- continuous_df %>%
  group_by(week) %>%
  mutate(
    portfolio = assign_portfolio(
      data = pick(everything()),
      sorting_variable = jr_neg,
      n_portfolios = 5
    ),
    portfolio = as.factor(portfolio)
  ) %>%
  group_by(portfolio, week) %>%
  summarize(
    ret_excess = mean(next_week_return),
    .groups = "drop"
  )

jr_cont_ar_ew <- JR_cont_portfolios_ew %>%
  group_by(portfolio) %>%
  summarize (
    avg_return = 10000 * mean(ret_excess, na.rm = TRUE),
    n_weeks = n(),
    .groups = "drop"
  )


# JR negative continuous sorted portfolio value weighted ------------------
JR_cont_portfolios_vw <- continuous_df %>%
  group_by(week) %>%
  mutate(
    portfolio = assign_portfolio(
      data = pick(everything()),
      sorting_variable = jr_neg,
      n_portfolios = 5
    ), 
    portfolio = as.factor(portfolio)
  ) %>%
  filter(!is.na(market_cap)) %>%
  group_by(portfolio, week) %>%
  summarize(
    ret_excess = weighted.mean(next_week_return, w = market_cap, na.rm = TRUE),
    .groups = "drop"
  )

jr_cont_ar_vw <- JR_cont_portfolios_vw %>% 
  group_by(portfolio) %>%
  summarize (
    avg_return = 10000 * mean(ret_excess, na.rm = TRUE),
    n_weeks = n(),
    .groups = "drop"
  )


# JR negative jump sorted portfolio equal weighted ------------------------
JR_jump_portfolios_ew <- jump_df %>%
  group_by(week) %>%
  mutate(
    portfolio = assign_portfolio(
      data = pick(everything()),
      sorting_variable = jr_neg,
      n_portfolios = 5
    ),
    portfolio = as.factor(portfolio)
  ) %>%
  group_by(portfolio, week) %>%
  summarize(
    ret_excess = mean(next_week_return),
    .groups = "drop"
  )

jr_jump_ar_ew <- JR_jump_portfolios_ew %>%
  group_by(portfolio) %>%
  summarize (
    avg_return = 10000 * mean(ret_excess, na.rm = TRUE),
    n_weeks = n(),
    .groups = "drop"
  )


# JR negative jump sorted portfolio value weighted ------------------------
JR_jump_portfolios_vw <- jump_df %>%
  group_by(week) %>%
  mutate(
    portfolio = assign_portfolio(
      data = pick(everything()),
      sorting_variable = jr_neg,
      n_portfolios = 5
    ), 
    portfolio = as.factor(portfolio)
  ) %>%
  filter(!is.na(market_cap)) %>%
  group_by(portfolio, week) %>%
  summarize(
    ret_excess = weighted.mean(next_week_return, w = market_cap, na.rm = TRUE),
    .groups = "drop"
  )

jr_jump_ar_vw <- JR_jump_portfolios_vw %>% 
  group_by(portfolio) %>%
  summarize (
    avg_return = 10000 * mean(ret_excess, na.rm = TRUE),
    n_weeks = n(),
    .groups = "drop"
  )


# Compute the spreads  ----------------------------------------------------

compute_spread <- function(df, return_col) {
  df %>%
    pivot_wider(names_from = portfolio, values_from = !!sym(return_col), names_prefix = "p") %>%
    mutate(spread = 10000 * (p5 - p1)) %>%
    select(week, spread)
}

jr_cont_spread_ew <- compute_spread(JR_cont_portfolios_ew, "ret_excess") 
jr_cont_spread_vw <- compute_spread(JR_cont_portfolios_vw, "ret_excess")
jr_jump_spread_ew <- compute_spread(JR_jump_portfolios_ew, "ret_excess")
jr_jump_spread_vw <- compute_spread(JR_jump_portfolios_vw, "ret_excess")


# Newey west for spreads --------------------------------------------------

nw_tstat <- function(spread_ts) {
  spread_ts <- spread_ts %>% filter(!is.na(spread))  # Remove NA values
  model <- lm(spread ~ 1, data = spread_ts)
  t_value <- coeftest(model, vcov. = NeweyWest(model))[1, "t value"]
  return(t_value)
}

# T-stats
jr_cont_tstat_ew <- nw_tstat(jr_cont_spread_ew)
jr_cont_tstat_vw <- nw_tstat(jr_cont_spread_vw)
jr_jump_tstat_ew <- nw_tstat(jr_jump_spread_ew)
jr_jump_tstat_vw <- nw_tstat(jr_jump_spread_vw)


# FFC4 for JR cont Equal Weighted Portfolio -------------------------------

JR_cont_with_factors_equal <- JR_cont_portfolios_ew %>%
  left_join(ffc4_factors, by = c("week" = "key")) %>%
  mutate(
    weekly_risk_free = trading_days_in_week * risk_free,
    ret_excess  = ret_excess - weekly_risk_free 
  )

ffc4_alpha_jr_cont_ew <- JR_cont_with_factors_equal %>%
  group_by(portfolio) %>%
  group_modify(~{
    model <- lm(ret_excess ~ mkt_excess + smb + hml + mom, data = .x)
    tidy(model, conf.int = TRUE, conf.level = 0.95) %>%
      filter(term == "(Intercept)") %>%
      select(estimate, std.error, statistic)
  })
ffc4_alpha_jr_cont_ew <- 10000 * ffc4_alpha_jr_cont_ew


# FFC4 for JR cont Value Weighted Portfolio -------------------------------
JR_cont_with_factors_value <- JR_cont_portfolios_vw %>%
  left_join(ffc4_factors, by = c("week" = "key")) %>%
  mutate(
    weekly_risk_free = trading_days_in_week * risk_free,
    ret_excess  = ret_excess - weekly_risk_free 
  )

ffc4_alpha_jr_cont_vw <- JR_cont_with_factors_value %>%
  group_by(portfolio) %>%
  group_modify(~{
    model <- lm(ret_excess ~ mkt_excess + smb + hml + mom, data = .x)
    tidy(model, conf.int = TRUE, conf.level = 0.95) %>%
      filter(term == "(Intercept)") %>%
      select(estimate, std.error, statistic)
  })
ffc4_alpha_jr_cont_vw <- 10000 * ffc4_alpha_jr_cont_vw


# FFC4 for JR jump equal weighted portfolio -------------------------------
JR_jump_with_factors_equal <- JR_jump_portfolios_ew %>%
  left_join(ffc4_factors, by = c("week" = "key")) %>%
  mutate(
    weekly_risk_free = trading_days_in_week * risk_free,
    ret_excess  = ret_excess - weekly_risk_free 
  )

ffc4_alpha_jr_jump_ew <- JR_jump_with_factors_equal %>%
  group_by(portfolio) %>%
  group_modify(~{
    model <- lm(ret_excess ~ mkt_excess + smb + hml + mom, data = .x)
    tidy(model, conf.int = TRUE, conf.level = 0.95) %>%
      filter(term == "(Intercept)") %>%
      select(estimate, std.error, statistic)
  })
ffc4_alpha_jr_jump_ew <- 10000 * ffc4_alpha_jr_jump_ew


# FFC4 for JR jump value weighted portfolio  ------------------------------

JR_jump_with_factors_value <- JR_jump_portfolios_vw %>%
  left_join(ffc4_factors, by = c("week" = "key")) %>%
  mutate(
    weekly_risk_free = trading_days_in_week * risk_free,
    ret_excess  = ret_excess - weekly_risk_free 
  )

ffc4_alpha_jr_jump_vw <- JR_jump_with_factors_value %>%
  group_by(portfolio) %>%
  group_modify(~{
    model <- lm(ret_excess ~ mkt_excess + smb + hml + mom, data = .x)
    tidy(model, conf.int = TRUE, conf.level = 0.95) %>%
      filter(term == "(Intercept)") %>%
      select(estimate, std.error, statistic)
  })
ffc4_alpha_jr_jump_vw <- 10000 * ffc4_alpha_jr_jump_vw


# Regressing the spreads on FFC4  -----------------------------------------
nw_tstat_FFC4 <- function(spreads) {
  # Run the regression of the spread on the FFC4 factors (including intercept)
  model <- lm(spread ~ mkt_excess + smb + hml + mom, data = spreads)
  
  # Get the Newey-West adjusted t-statistic for the intercept
  t_stat_intercept <- coeftest(model, vcov = NeweyWest(model))["(Intercept)", "t value"]
  
  # Return the t-statistic for the intercept
  return(t_stat_intercept)
}

# Group each spread with the FFC4 data 

grouped_data_jr_cont_ew <- jr_cont_spread_ew %>%
  left_join(ffc4_factors, by = c("week" = "key")) %>%
  group_by(week)

grouped_data_jr_cont_vw <- jr_cont_spread_vw %>%
  left_join(ffc4_factors, by = c("week" = "key")) %>%
  group_by(week)

grouped_data_jr_jump_ew <- jr_jump_spread_ew %>%
  left_join(ffc4_factors, by = c("week" = "key")) %>%
  group_by(week)

grouped_data_jr_jump_vw <- jr_jump_spread_vw %>%
  left_join(ffc4_factors, by = c("week" = "key")) %>%
  group_by(week)

jr_cont_alpha_tstat_ew <- nw_tstat_FFC4(grouped_data_jr_cont_ew)
jr_cont_alpha_tstat_vw <- nw_tstat_FFC4(grouped_data_jr_cont_vw)
jr_jump_alpha_tstat_ew <- nw_tstat_FFC4(grouped_data_jr_jump_ew)
jr_jump_alpha_tstat_vw <- nw_tstat_FFC4(grouped_data_jr_jump_vw)


