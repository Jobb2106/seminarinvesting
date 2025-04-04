# This script can be used to calculate the Adjusted jump-ratio test

# Import packages ---------------------------------------------------------
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)


# Import data -------------------------------------------------------------
file_paths <- list.files("data/subset", pattern = "^filtered_\\d{4}-W\\d{2}\\.rds$", full.names = TRUE)
output_folder <- "data/metrics/weekly_ajr"


# Numeric values ----------------------------------------------------------
v = 0.6090 
mu = sqrt(2 / pi)
M = 78 
delta = 1/M 


# Calculate the realized BiPower variation --------------------------------
BPV <- function(returns){
  abs_returns <- abs(returns)
  return(sum(abs_returns[-1] * abs_returns[-length(abs_returns)]))
}


# Calculate the RV --------------------------------------------------------
RV <- function(returns) {
  sum(returns^2)
}


# Calculate the realized QuadPower variation ------------------------------
QPV <- function(returns){
  abs_returns <- abs(returns)
  return(M * sum(
    abs_returns[1:(length(abs_returns)-3)] *
      abs_returns[2:(length(abs_returns)-2)] *
      abs_returns[3:(length(abs_returns)-1)] *
      abs_returns[4:length(abs_returns)]
  ))
}


# Calculate the adjusted jump-ratio test statistic ------------------------
AJR <- function(RV, BPV, QPV, M, mu) {
  wortel <- max(1, QPV / (BPV^2))
  (sqrt(M) / sqrt(wortel)) * ((mu^(-2)) * BPV / RV - 1)
}


# Process weeks -----------------------------------------------------------
process_weekly_ajr <- function(file_path) {
  df <- readRDS(file_path)
  week_id <- str_extract(basename(file_path), "20\\d{2}-W\\d{2}")
  
  df_ajr <- df %>%
    mutate(
      rv = map_dbl(returns_5m, RV),
      bpv = map_dbl(returns_5m, BPV),
      qpv = map_dbl(returns_5m, QPV),
      ajr = mapply(AJR, rv, bpv, qpv, MoreArgs = list(M = M, mu = mu))
    ) %>%
    filter(!is.na(ajr)) %>%
    group_by(sym_root) %>%
    summarise(
      week = week_id,
      ajr_sum = sum(ajr, na.rm = TRUE),
      ajr_mean = mean(ajr, na.rm = TRUE),
      n_days = n(),
      .groups = "drop"
    ) %>%
    mutate(
      ajr_median = median(ajr_sum, na.rm = TRUE),
      portfolio = if_else(ajr_sum < ajr_median, "jump", "continuous")
    ) %>%
    select(-ajr_median)  # optional: remove if not needed
  
  # Save result
  saveRDS(df_ajr, file.path(output_folder, paste0("ajr_", week_id, ".rds")))
  cat("Saved AJR + portfolios for", week_id, "\n")
}


# Apply to all ------------------------------------------------------------
walk(file_paths, process_weekly_ajr)

