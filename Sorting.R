# This script can be used for portfolio sorting
# Deze link van TidyVerse heb ik vooral gebruikt: https://www.tidy-finance.org/r/univariate-portfolio-sorts.html

# TODO ook sorten op Realized Quantiles & Double-Sorten

# Data --------------------------------------------------------------------
# Laad de data uit GitHub
file_paths <- list.files("data/subset", pattern = "^filtered_\\d{4}-W\\d{2}\\.rds$", full.names = TRUE)


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
  sorting_vec <- data |> pull({{ sorting_variable }})
  
  breakpoints <- quantile(
    sorting_vec,
    probs = seq(0, 1, length.out = n_portfolios + 1),
    na.rm = TRUE,
    names = FALSE
  )
  
  data <- data |> 
    mutate(portfolio = findInterval(sorting_vec, breakpoints, all.inside = TRUE))
  
  return(data$portfolio)
}


# Return for next week ----------------------------------------------------
# Including the next week return for the dropped companies
add_next_week_return <- function(current_week_df, next_week_df, dropped_df) {
  # Step 1: Pull returns from next_week_df
  next_returns <- next_week_df %>%
    select(permno, returns_week) %>%
    rename(next_week_return = returns_week)
  
  # Step 2: Left join to current week data
  merged_df <- current_week_df %>%
    left_join(next_returns, by = "permno")
  
  # Step 3: If any returns are missing, try filling from dropped_df
  if (!is.null(dropped_df) && any(is.na(merged_df$next_week_return))) {
    fallback_returns <- dropped_df %>%
      select(permno, returns_week) %>%
      rename(next_week_return = returns_week)
    
    merged_df <- merged_df %>%
      left_join(fallback_returns, by = "permno", suffix = c("", ".fallback")) %>%
      mutate(next_week_return = coalesce(next_week_return, next_week_return.fallback)) %>%
      select(-next_week_return.fallback)
  }
  
  return(merged_df)
}

# Performance Evaluation --------------------------------------------------
summarise_portfolios <- function(df) {
  portfolio_avg <- df %>%
    group_by(portfolio) %>%
    summarise(avg_return = mean(next_week_return, na.rm = TRUE), .groups = "drop")
  
  spread <- portfolio_avg$avg_return[portfolio_avg$portfolio == 5] -
    portfolio_avg$avg_return[portfolio_avg$portfolio == 1]
  
  tibble(P1 = portfolio_avg$avg_return[portfolio_avg$portfolio == 1],
         P5 = portfolio_avg$avg_return[portfolio_avg$portfolio == 5],
         spread = spread)
}


# Performance Evaluation --------------------------------------------------

# Moeten deze data nog ergens vandaan toveren, die code zou niet heel lastig moeten zijn
# Run Fama-French (heb nu FFC4) regression per portfolio

ffc_results <- ffc_data %>%
  group_by(portfolio) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(excess_ret ~ MKT_RF + SMB + HML + MOM, data = .x)),
    robust_se = map(model, ~ vcovHC(.x, type = "HC1")),
    coefs = map2(model, robust_se, ~ coeftest(.x, vcov = .y) %>% tidy())
  ) %>%
  unnest(coefs) %>%
  filter(term == "(Intercept)") %>%
  select(portfolio, estimate, std.error, statistic, p.value)

# Dan tenslotte weer de High Low spread ook voor deze


