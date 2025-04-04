# This is where the data magic happens

# Load packages -----------------------------------------------------------
library(dplyr)
library(purrr)
library(tidyverse)
library(stringr)
library(readr)
library(lubridate)


# Set folders and files -------------------------------------------------------------
rds_folder <- "input/cleaned"
#rds_folder <- "/Users/job/Desktop/RDS"
#rds_folder <- "/Users/thorhogerbrugge/Desktop/RDS"
rds_files <- list.files(rds_folder, pattern = "\\.rds$", full.names = TRUE)


# Define functions -------------------------------------------------------------------
extract_date <- function(path) {
  str_extract(basename(path), "\\d{4}-\\d{2}-\\d{2}")
}

extract_year <- function(date_str) {
  format(as.Date(date_str), "%Y")
}

week_key <- function(date) {
  shifted_date <- as.Date(date) - days(1)
  paste0(isoyear(shifted_date), "-W", sprintf("%02d", isoweek(shifted_date)))
}

# Group files by week -----------------------------------------------------
file_dates <- as.Date(sapply(rds_files, extract_date))
week_ids <- week_key(file_dates)
files_by_week <- split(rds_files, week_ids)


# Parameters for filtering ------------------------------------------------
min_percent_days <- 0.8 #zelf bedacht
min_price <- 5 #paper
max_price <- 1000 #common
min_n_obs <- 80 #(good bad volatility paper) staat er niet echt in


# Stock universes per week ------------------------------------------------
get_eligible_stocks_weekly <- function(file_paths, min_price, max_price, min_n_obs, min_percent_days) {
  if (length(file_paths) == 0) return(character(0))
  
  all_data <- map_dfr(file_paths, function(file) {
    tryCatch(readRDS(file), error = function(e) {
      cat("Skipping file:", path, "due to error:", conditionMessage(e), "\n")
      return(NULL)
    })})
  
  # Count trading days per stock
  trading_days_per_stock <- all_data %>%
    group_by(sym_root) %>%
    summarise(days_available = n(), .groups = "drop")
  
  # Find max number of trading days across all stocks this week
  max_days <- max(trading_days_per_stock$days_available, na.rm = TRUE)
  required_days <- floor(min_percent_days * max_days)
  
  # Count number of available days per stock
  enough_data <- trading_days_per_stock %>%
    filter(days_available >= required_days)
  
  failing_stocks <- all_data %>%
    filter(sym_root %in% enough_data$sym_root) %>%
    filter(
      is.na(close_crsp) |
        close_crsp < min_price |
        close_crsp > max_price |
        n_obs < min_n_obs
    ) %>%
    pull(sym_root) %>%
    unique()
  
  eligible <- setdiff(enough_data$sym_root, failing_stocks)
  return(eligible)
}

# Build universes for each week---------------------------------------
week_list <- names(files_by_week)
universe_by_week <- list()

for (i in 1:(length(week_list) - 1)) {
  current_week <- week_list[i]
  next_week <- week_list[i + 1]
  
  cat("Building universe for", next_week, "using", current_week, "data...\n")
  
  file_paths <- files_by_week[[current_week]]
  
  eligible <- get_eligible_stocks_weekly(
    file_paths,
    min_price = min_price,
    max_price = max_price,
    min_n_obs = min_n_obs,
    min_percent_days = min_percent_days
  )

  universe_by_week[[next_week]] <- eligible
  
  # Save the filtered data for next_week
  df_week <- map_dfr(files_by_week[[next_week]], function(file) {
    date_str <- str_extract(basename(file), "\\d{4}-\\d{2}-\\d{2}")
    tryCatch({
      readRDS(file) %>% mutate(date = as.Date(date_str))
    }, error = function(e) NULL)
  })
  
  df_filtered <- df_week %>%
    filter(sym_root %in% eligible) %>%
    filter(
      !is.na(close_crsp),
      close_crsp >= min_price,
      close_crsp <= max_price,
      n_obs >= min_n_obs
    )
  
  saveRDS(df_filtered, paste0("data/subset/filtered_", next_week, ".rds"))
  
  # Dropped stocks (present last week, not this week)
  if (!is.null(universe_by_week[[current_week]])) {
    dropped_stocks <- setdiff(universe_by_week[[current_week]], eligible)
    
    if (length(dropped_stocks) > 0) {
      # Save data of dropped stocks
      dropped_data <- df_week %>% filter(sym_root %in% dropped_stocks)
      saveRDS(dropped_data, paste0("data/setdifference/dropped_data_", next_week, ".rds"))
    }
  }
}

#if (!dir.exists("data/weekly_filtered")) dir.create("data/weekly_filtered")
#if (!dir.exists("data/setdifference")) dir.create("data/setdifference")
