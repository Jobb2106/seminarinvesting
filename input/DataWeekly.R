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
min_days <- 3 #good bad vol paper
min_price <- 5 #paper
max_price <- 1000 #common
min_n_obs <- 80 #(good bad volatility paper) staat er niet echt in


# Stock universes per week ------------------------------------------------
get_eligible_stocks_weekly <- function(file_paths, min_days, min_price, max_price, min_n_obs) {
  if (length(file_paths) == 0) return(character(0))
  
  all_data <- map_dfr(file_paths, function(file) {
    tryCatch(readRDS(file), error = function(e) {
      cat("Skipping file:", file, "due to error:", conditionMessage(e), "\n")
      return(NULL)
    })
  })
  
  # Filter for per-day quality constraints
  valid_data <- all_data %>%
    filter(
      !is.na(close_crsp),
      close_crsp >= min_price,
      close_crsp <= max_price,
      n_obs >= min_n_obs
    )
  
  # Count number of valid days each stock appears
  stock_day_counts <- valid_data %>%
    group_by(permno) %>%
    summarise(n_valid_days = n_distinct(date), .groups = "drop")
  
  # Only keep stocks with enough valid days
  eligible_stocks <- stock_day_counts %>%
    filter(n_valid_days >= min_days) %>%
    pull(permno)
  
  return(eligible_stocks)
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
    min_days = min_days,
    min_price = min_price,
    max_price = max_price,
    min_n_obs = min_n_obs
  )

  universe_by_week[[next_week]] <- eligible
  
  # Save the filtered data for next_week
  df_week <- map_dfr(files_by_week[[next_week]], function(file) {
    date_str <- str_extract(basename(file), "\\d{4}-\\d{2}-\\d{2}")
    tryCatch({
      readRDS(file) %>% mutate(date = as.Date(date_str))
    }, error = function(e) NULL)
  })
  
  total_days <- df_week %>%
    pull(date) %>%
    unique() %>%
    length()
  
  # Calculate required number of valid days per stock (based on 80% threshold)
  required_days <- floor(0.8 * total_days)
  
  # Only keep eligible stocks that appear on enough days in df_week
  valid_counts <- df_week %>%
    filter(permno %in% eligible) %>%
    filter(
      !is.na(close_crsp),
      close_crsp >= min_price,
      close_crsp <= max_price,
      n_obs >= min_n_obs
    ) %>%
    group_by(permno) %>%
    summarise(n_days = n_distinct(date), .groups = "drop") %>%
    filter(n_days >= required_days)
  
  df_filtered <- df_week %>%
    filter(
      permno %in% valid_counts$permno,
      !is.na(close_crsp),
      close_crsp >= min_price,
      close_crsp <= max_price,
      n_obs >= min_n_obs
    )
  
  saveRDS(df_filtered, paste0("data/subset/filtered_", next_week, ".rds"))
  
  # Dropped stocks (present last week, not this week)
  if (!is.null(universe_by_week[[current_week]])) {
    dropped_stocks <- setdiff(universe_by_week[[current_week]], universe_by_week[[next_week]])
    
    if (length(dropped_stocks) > 0) {
      # Save data of dropped stocks
      dropped_data <- df_week %>% filter(permno %in% dropped_stocks)
      saveRDS(dropped_data, paste0("data/setdifference/dropped_data_", next_week, ".rds"))
    }
  }
}

#if (!dir.exists("data/weekly_filtered")) dir.create("data/weekly_filtered")
#if (!dir.exists("data/setdifference")) dir.create("data/setdifference")
