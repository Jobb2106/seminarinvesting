# This is where the data magic happens

# Load packages -----------------------------------------------------------
library(dplyr)
library(purrr)
library(tidyverse)
library(stringr)
library(readr)
library(lubridate)


# Set folders and files -------------------------------------------------------------
rds_folder <- "input/cleanedsubset"
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
min_percent_days <- 0.8 #good bad vol paper
min_price <- 5 #paper
max_price <- 1000 #common
min_n_obs <- 80 #(good bad volatility paper) staat er niet echt in


# Universe builder per week -----------------------------------------------
filter_week_data <- function(file_paths, min_price, max_price, min_n_obs, min_percent_days) {
  all_data <- map_dfr(file_paths, function(file) {
    tryCatch(readRDS(file), error = function(e) {
      cat("Skipping file:", file, "due to error:", conditionMessage(e), "\n")
      return(NULL)
    })
  })
  
  total_days <- n_distinct(all_data$date)
  required_days <- floor(min_percent_days * total_days)
  
  filtered <- all_data %>%
    filter(
      !is.na(close_crsp),
      close_crsp >= min_price,
      close_crsp <= max_price,
      n_obs >= min_n_obs
    ) %>%
    group_by(permno) %>%
    filter(n_distinct(date) >= required_days) %>%
    ungroup()
  
  return(filtered)
}


# Build and save weekly universes ----------------------------------------
week_list <- names(files_by_week)
universe_by_week <- list()

for (i in 1:(length(week_list) - 1)) {
  current_week <- week_list[i]
  next_week <- week_list[i + 1]
  
  cat("Building universe for", next_week, "using", current_week, "data...\n")
  
  current_paths <- files_by_week[[current_week]]
  next_paths <- files_by_week[[next_week]]
  
  filtered_current <- filter_week_data(current_paths, min_price, max_price, min_n_obs, min_percent_days)
  filtered_next <- filter_week_data(next_paths, min_price, max_price, min_n_obs, min_percent_days)
  
  universe_by_week[[next_week]] <- unique(filtered_current$permno)
  
  saveRDS(filtered_next, paste0("data/subset/filtered_", next_week, ".rds"))
  
  # Dropped stocks (present last week, not in this week's universe)
  if (!is.null(universe_by_week[[current_week]])) {
    dropped_permnos <- setdiff(universe_by_week[[current_week]], universe_by_week[[next_week]])
    
    if (length(dropped_permnos) > 0) {
      # Use unfiltered data for next week, not filtered_next
      raw_next_week <- map_dfr(files_by_week[[next_week]], function(file) {
        date_str <- str_extract(basename(file), "\\d{4}-\\d{2}-\\d{2}")
        tryCatch({
          readRDS(file) %>% mutate(date = as.Date(date_str))
        }, error = function(e) NULL)
      })
      
      dropped_data <- raw_next_week %>% filter(permno %in% dropped_permnos)
      saveRDS(dropped_data, paste0("data/setdifference/dropped_data_", next_week, ".rds"))
    }
  }
}

#if (!dir.exists("data/weekly_filtered")) dir.create("data/weekly_filtered")
#if (!dir.exists("data/setdifference")) dir.create("data/setdifference")
