# This script can be used for portfolio sorting
# Deze link van TidyVerse heb ik vooral gebruikt: https://www.tidy-finance.org/r/univariate-portfolio-sorts.html

# TODO ook sorten op Realized Quantiles & Double-Sorten

# Data --------------------------------------------------------------------
# Laad de data uit GitHub
file_paths <- list.files("data/subset", pattern = "^filtered_\\d{4}-W\\d{2}\\.rds$", full.names = TRUE)

# FFC4 factors
ffc4_factors <- readRDS("data/metrics/FFC4.rds") %>%
  mutate(key = as.character(key))

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(RSQLite)
library(scales)
library(lmtest)
library(broom)
library(sandwich)
library(tidyr)
library(dplyr)


# Portfolio Sorting ----------------------------------------------------
assign_portfolio <- function(data, sorting_variable, n_portfolios) {
  # Compute breakpoints for the given sorting variable by week
  breakpoints <- data %>%
    pull({{ sorting_variable }}) %>%
    quantile(
      probs = seq(0, 1, length.out = n_portfolios + 1),
      na.rm = TRUE, names = FALSE
    )
  # Assign portfolios using findInterval()
  data %>%
    mutate(portfolio = findInterval(pick(everything()) |> 
                                      pull({{ sorting_variable }}), 
                                    breakpoints, all.inside = TRUE)
    ) %>%
    pull(portfolio)
}

# Equally weighted returns ------------------------------------------------
summarise_portfolios <- function(df) {
  df %>%
    pivot_longer(
      cols = c(RSJ_portfolio, RES_portfolio),   # Pivot the two portfolio columns into one
      names_to = "sort_type", 
      values_to = "portfolio"
    ) %>%
    group_by(sort_type, portfolio) %>%
    summarise(
      avg_return = mean(next_week_return, na.rm = TRUE),
      .groups = "drop"
    )
}


# Value Weighted Returns --------------------------------------------------
summarise_portfolios_value_weighted <- function(df) {
  df %>%
    pivot_longer(
      cols = c(RSJ_portfolio, RES_portfolio),
      names_to = "sort_type", 
      values_to = "portfolio"
    ) %>%
    group_by(sort_type, portfolio) %>%
    summarise(
      avg_return = weighted.mean(next_week_return, w = market_cap, na.rm = TRUE),
      .groups = "drop"
    )
}



# Single Sorting ----------------------------------------------------------
weekly_all <- bind_rows(weekly_results) %>% 
  mutate(week = as.character(week)) %>% 
  select(week, permno, RSJ_week, RES_week, market_cap, next_week_return)

# Compute portfolios using RSJ_week sorting 
rsj_portfolios <- weekly_all %>%
  group_by(week) %>%
  mutate(
    portfolio = assign_portfolio(pick(everything()), RSJ_week, n_portfolios = 5),
    portfolio = as.factor(portfolio)
  ) %>%
  ungroup() %>%
  group_by(week, portfolio) %>%
  summarize(
    ret_excess_weighted = weighted.mean(next_week_return, market_cap, na.rm = TRUE),
    ret_excess_equal = mean(next_week_return, na.rm = TRUE),
    .groups = "drop"
  )

# Results
average_returns_by_portfolio_rsj <- rsj_portfolios %>%
  group_by(portfolio) %>%
  summarize(
    avg_weighted_return = mean(ret_excess_weighted, na.rm = TRUE),
    avg_equal_return    = mean(ret_excess_equal, na.rm = TRUE),
    n_weeks             = n(),
    .groups = "drop"
  )

# 2. Compute value‐weighted portfolios using RES_week sorting
res_portfolios <- weekly_all %>%
  group_by(week) %>%
  mutate(
    portfolio = assign_portfolio(pick(everything()), RES_week, n_portfolios = 5),
    portfolio = as.factor(portfolio)
  ) %>%
  ungroup() %>%
  group_by(week, portfolio) %>%
  summarize(
    ret_excess_weighted = weighted.mean(next_week_return, market_cap, na.rm = TRUE),
    ret_excess_equal = mean(next_week_return, na.rm = TRUE),
    .groups = "drop"
  )

average_returns_by_portfolio_rsj <- res_portfolios %>%
  group_by(portfolio) %>%
  summarize(
    avg_weighted_return = mean(ret_excess_weighted, na.rm = TRUE),
    avg_equal_return    = mean(ret_excess_equal, na.rm = TRUE),
    n_weeks             = n(),
    .groups = "drop"
  )

# Spread Calculation ------------------------------------------------------
compute_spread <- function(portfolio_data, ret_type = c("weighted", "equal")) {
  ret_type <- match.arg(ret_type)
  
  ret_col <- if(ret_type == "weighted") "ret_excess_weighted" else "ret_excess_equal"
  
  portfolio_data %>%
    # Create wide format: one column per portfolio; names prefixed by "P"
    pivot_wider(names_from = portfolio, values_from = all_of(ret_col),
                names_prefix = "P") %>%
    # Compute spread as portfolio 5 minus portfolio 1.
    mutate(spread = P5 - P1) %>%
    select(week_id, spread)
}

# For RSJ sort:
rsj_spread_weighted <- compute_spread(rsj_portfolios, ret_type = "weighted")
rsj_spread_equal    <- compute_spread(rsj_portfolios, ret_type = "equal")

# For RES sort (assuming you computed res_portfolios similarly):
res_spread_weighted <- compute_spread(res_portfolios, ret_type = "weighted")
res_spread_equal    <- compute_spread(res_portfolios, ret_type = "equal")



# Newey-West tstat ---------------------------------------------------------
nw_tstat <- function(spread_ts) {
  model <- lm(spread ~ 1, data = spread_ts)
  t_value <- coeftest(model, vcov. = NeweyWest(model))[1, "t value"]
  return(t_value)
}

rsj_tstat_ew <- nw_tstat(rsj_spread_equal)
rsj_tstat_vw <- nw_tstat(rsj_spread_weighted)
res_tstat_ew <- nw_tstat(res_spread_equal)
res_tstat_vw <- nw_tstat(res_spread_weighted)

# FFC4 --------------------------------------------------------------------
# Step 3: Join with factor data (assuming ffc4_factors has a key column named "key")
returns_with_factors_equal <- rsj_portfolios %>%
  left_join(ffc4_factors, by = c("week" = "key"))

# Step 4: For each sort_type and portfolio, run the FFC4 regression on the equal-weighted portfolio returns
ffc4_alpha_results_equal <- returns_with_factors_equal %>%
  group_by(portfolio) %>%
  group_modify(~{
    model <- lm(ret_excess_equal ~ mkt_excess + smb + hml + mom, data = .x)
    tidy(model, conf.int = TRUE, conf.level = 0.95) %>%
      filter(term == "(Intercept)") %>%
      select(estimate, std.error, statistic)
  })
