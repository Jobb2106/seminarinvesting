# Filter data and group by weeks

# Import packages ---------------------------------------------------------
library(dplyr)
library(purrr)
library(tidyverse)
library(stringr)
library(readr)
library(lubridate)


# Set input and output paths ----------------------------------------------
rds_folder        <- "E:/Seminar/Cleaned"
filtered_out_path <- "E:/Seminar/Weekly"
dropped_out_path  <- "E:/Seminar/Dropped"
rejected_out_path <- "E:/Seminar/Rejected"  # optional folder for all rejected stocks

# Create output folders if they do not exist
dir.create(filtered_out_path, showWarnings = FALSE, recursive = TRUE)
dir.create(dropped_out_path, showWarnings = FALSE, recursive = TRUE)
dir.create(rejected_out_path, showWarnings = FALSE, recursive = TRUE)

# List all RDS files
rds_files <- list.files(rds_folder, pattern = "\\.rds$", full.names = TRUE)


# Functions -------------------------------------------------
extract_date <- function(path) {
  str_extract(basename(path), "\\d{4}-\\d{2}-\\d{2}")
}

week_key <- function(date) {
  shifted_date <- as.Date(date) - days(1)
  paste0(isoyear(shifted_date), "-W", sprintf("%02d", isoweek(shifted_date)))
}


# Get relevant data -------------------------------------------------------
file_dates <- as.Date(sapply(rds_files, extract_date))
week_ids <- week_key(file_dates)
# Each element: a vector of file paths for that week
files_by_week <- split(rds_files, week_ids)

# Ensure week_list is sorted in chronological order
week_list <- sort(names(files_by_week))
cat("Weeks to process:", week_list, "\n\n")


# Parameters for filtering ------------------------------------------------
min_price <- 5
max_price <- 1000
min_n_obs <- 80



# Filtering function ------------------------------------------------------
# A stock is kept only if it appears on ALL trading days in the week
# and every observation meets the conditions.
filter_week_data <- function(file_paths, min_price, max_price, min_n_obs) {
  # Load all data for the week
  all_data <- map_dfr(file_paths, function(file) {
    tryCatch(
      readRDS(file),
      error = function(e) {
        cat("Skipping file:", file, "due to error:", conditionMessage(e), "\n")
        return(NULL)
      }
    )
  })
  
  # Determine the number of unique trading days in the week
  week_dates <- unique(all_data$date)
  total_trading_days <- length(week_dates)
  required_days <- floor(0.8 * total_trading_days)
  
  # Split data by stock (using permno as the unique identifier)
  stock_split <- split(all_data, all_data$permno)
  
  # For each stock, require it to appear on all trading days and that every observation passes all conditions.
  valid_stocks <- keep(stock_split, function(df) {
    # First, check that the stock is observed on every trading day in the week.
    if (n_distinct(df$date) < required_days) return(FALSE)
    # Then, ensure no key values are missing.
    if (any(is.na(df$close_crsp)) || any(is.na(df$n_obs))) return(FALSE)
    # Finally, check all rows meet the criteria.
    all(df$close_crsp >= min_price & df$close_crsp <= max_price & df$n_obs >= min_n_obs)
  })
  
  filtered <- bind_rows(valid_stocks)
  return(filtered)
}


# Process weeks -----------------------------------------------------------
# Initialize a list to store the universe for each week (set of permnos that passed filtering)
universe_by_week <- list()

# Process first week separately (no dropped computation for the first week)
first_week <- week_list[1]
cat("Processing first week:", first_week, "\n")
filtered_first <- filter_week_data(files_by_week[[first_week]], min_price, max_price, min_n_obs)
universe_by_week[[first_week]] <- unique(filtered_first$permno)
# Save the filtered data for the first week
saveRDS(filtered_first, file = file.path(filtered_out_path, paste0("filtered_", first_week, ".rds")))

# Save rejected raw data (for stocks that did not meet filtering)
raw_first <- map_dfr(files_by_week[[first_week]], function(file) {
  date_str <- str_extract(basename(file), "\\d{4}-\\d{2}-\\d{2}")
  tryCatch({
    readRDS(file) %>% mutate(date = as.Date(date_str))
  }, error = function(e) NULL)
})
raw_permnos_first <- unique(raw_first$permno)
rejected_first <- raw_first %>% filter(!(permno %in% universe_by_week[[first_week]]))
if(nrow(rejected_first) > 0){
  saveRDS(rejected_first, file = file.path(rejected_out_path, paste0("rejected_", first_week, ".rds")))
}

# Process remaining weeks
for (i in 2:length(week_list)) {
  current_week <- week_list[i]
  prev_week <- week_list[i - 1]
  
  cat("\nBuilding universe for", current_week, "using", prev_week, "data...\n")
  
  # Filter current week raw data
  filtered_current <- filter_week_data(files_by_week[[current_week]], min_price, max_price, min_n_obs)
  current_universe <- unique(filtered_current$permno)
  
  # Save the filtered data for the current week
  saveRDS(filtered_current, file = file.path(filtered_out_path, paste0("filtered_", current_week, ".rds")))
  
  # Load raw data for the current week (without filtering) to count observations per stock
  raw_current <- map_dfr(files_by_week[[current_week]], function(file) {
    date_str <- str_extract(basename(file), "\\d{4}-\\d{2}-\\d{2}")
    tryCatch({
      readRDS(file) %>% mutate(date = as.Date(date_str))
    }, error = function(e) NULL)
  })
  
  # Compute dropped stocks: those that were in the previous week’s universe but are absent in the current week’s filtered universe.
  dropped_permnos <- setdiff(universe_by_week[[prev_week]], current_universe)
  
  if (length(dropped_permnos) > 0) {
    # Save raw data for the current week for the dropped stocks.
    dropped_data <- raw_current %>% filter(permno %in% dropped_permnos)
    saveRDS(dropped_data, file = file.path(dropped_out_path, paste0("dropped_data_", current_week, ".rds")))
  }
  
  # Optional: Save rejected raw data (for stocks that were observed raw but did not pass filtering)
  raw_permnos_current <- unique(raw_current$permno)
  rejected_permnos <- setdiff(raw_permnos_current, current_universe)
  if (length(rejected_permnos) > 0) {
    rejected_data <- raw_current %>% filter(permno %in% rejected_permnos)
    saveRDS(rejected_data, file = file.path(rejected_out_path, paste0("rejected_", current_week, ".rds")))
  }
  
  # Update the universe for the current week
  universe_by_week[[current_week]] <- current_universe
}
