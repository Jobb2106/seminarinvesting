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
  sorting_vec <- as.numeric(data |> pull({{ sorting_variable }}))
  
  breakpoints <- quantile(
    sorting_vec,
    probs = seq(0, 1, length.out = n_portfolios + 1),
    na.rm = TRUE,
    names = FALSE
  )
  
  # Maak de portfolio-indeling en retourneer deze vector
  data <- data |> 
    mutate(portfolio = findInterval(sorting_vec, breakpoints, all.inside = TRUE))
  
  return(data$portfolio)
}



# Add next week return ----------------------------------------------------
add_next_week_return <- function(current_week_df, next_week_df, dropped_df) {
  # Stap 1: Haal returns op uit next_week_df
  next_returns <- next_week_df %>%
    select(permno, returns_week) %>%
    rename(next_week_return = returns_week)
  
  # Stap 2: Left join met de data van de huidige week
  merged_df <- current_week_df %>%
    left_join(next_returns, by = "permno")
  
  # Stap 3: Indien er returns ontbreken, vul in met data uit dropped_df
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


