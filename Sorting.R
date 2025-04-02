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
  breakpoints <- data |>
    pull({{ sorting_variable }}) |>
    quantile(
      probs = seq(0, 1, length.out = n_portfolios + 1),
      na.rm = TRUE,
      names = FALSE
    )
  
  data <- data |>
    mutate(portfolio = findInterval(data[[ sorting_variable ]], breakpoints, all.inside = TRUE))
  
  return(data$portfolio)
}


# Return for next week ----------------------------------------------------
add_next_week_return <- function(current_week_df, next_week_df, week_id, dropped_data_path) {
  # Step 1: Get primary next week return data
  next_returns <- next_week_df %>%
    select(permno, returns_week) %>%
    rename(next_week_return = returns_week)
  
  # Step 2: Merge into current week data
  merged_df <- current_week_df %>%
    left_join(next_returns, by = "permno")
  
  # Step 3: Check if there are any missing next_week_return
  if (any(is.na(merged_df$next_week_return))) {
    
    # Construct fallback filename
    fallback_file <- file.path(dropped_data_path, paste0("dropped_data_", week_id, ".rds"))
    
    if (file.exists(fallback_file)) {
      dropped_data <- readRDS(fallback_file)
      
      # Clean and rename fallback
      fallback_returns <- dropped_data %>%
        select(permno, returns_week) %>%
        rename(next_week_return = returns_week)
      
      # Fill in missing values using coalesce
      merged_df <- merged_df %>%
        left_join(fallback_returns, by = "permno", suffix = c("", ".fallback")) %>%
        mutate(next_week_return = coalesce(next_week_return, next_week_return.fallback)) %>%
        select(-next_week_return.fallback)
    } else {
      warning(paste("Fallback file not found:", fallback_file))
    }
  }
  
  return(merged_df)
}



# Performance Evaluation --------------------------------------------------
summarise_portfolios <- function(df) {
  df %>%
    group_by(portfolio) %>%
    group_split() %>%
    purrr::map_dfr(function(group_df) {
      p <- unique(group_df$portfolio)
      x <- group_df$next_week_return
      n <- sum(!is.na(x))
      
      if (n < 4 || is.na(p)) {
        return(tibble(
          portfolio = p,
          avg_next_return = mean(x, na.rm = TRUE),
          n_obs = n,
          nw_se = NA_real_,
          t_stat = NA_real_
        ))
      }
      
      model <- lm(x ~ 1)
      nw <- coeftest(model, vcov = NeweyWest)
      tibble(
        portfolio = p,
        avg_next_return = nw[1, "Estimate"],
        n_obs = n,
        nw_se = nw[1, "Std. Error"],
        t_stat = nw[1, "t value"]
      )
    })
}

# High-low spread
calculate_weekly_spreads <- function(df, week_id) {
  df %>%
    filter(portfolio %in% c(1, 5)) %>%
    select(portfolio, next_week_return) %>%
    group_by(portfolio) %>%
    summarise(next_week_return = mean(next_week_return, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = portfolio, values_from = next_week_return, names_prefix = "P") %>%
    mutate(spread_P5_P1 = P5 - P1, week_id = week_id)
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


