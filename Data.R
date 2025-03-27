# This is where the data magic happens

# Load packages -----------------------------------------------------------
library(dplyr)
library(purrr)
library(tidyverse)
library(stringr)

# Set folders and files -------------------------------------------------------------
#rds_folder <- "/Users/job/Desktop/RDS"
rds_folder <- "/Users/job/Desktop/RDS3"
#rds_folder <- "/Users/thorhogerbrugge/Desktop/RDS"
rds_files <- list.files(rds_folder, pattern = "\\.rds$", full.names = TRUE)


# Extract date and year -------------------------------------------------------------------
extract_date <- function(path) {
  str_extract(basename(path), "\\d{4}-\\d{2}-\\d{2}")
}

extract_year <- function(date_str) {
  format(as.Date(date_str), "%Y")
}


# Group files by year -----------------------------------------------------
file_dates <- as.Date(sapply(rds_files, extract_date))
files_by_year <- split(rds_files, format(file_dates, "%Y"))


# Parameters for filtering ------------------------------------------------
min_days <- 200 #chatgpt
min_price <- 5 #paper
max_price <- 1000 #common
min_n_obs <- 390 #1transactie per minuut volgens literatuur


# Stock universes ------------------------------------------------------------------
get_eligible_stocks <- function(file_paths, min_days, min_price, max_price, min_n_obs) {
  safe_read <- function(path) {
    tryCatch(readRDS(path), error = function(e) {
      cat("Skipping file:", path, "due to error:", conditionMessage(e), "\n")
      return(NULL)
    })
  } 
  
  all_data <- map_dfr(file_paths, safe_read)
  
  filtered <- all_data %>%
    filter(!is.na(close_crsp),
           close_crsp >= min_price,
           close_crsp <= max_price,
           n_obs >= min_n_obs)
  
  stock_counts <- filtered %>%
    group_by(sym_root) %>%
    summarise(valid_days = n(), .groups = "drop") %>%
    filter(valid_days >= min_days)
  
  return(stock_counts$sym_root)
}

years <- as.character(1993:2023)
universe_by_year <- list()

for (yr in years) {
  cat("Computing stock universe for", yr, "...\n")
  file_paths <- files_by_year[[yr]]
  if (!is.null(file_paths)) {
    eligible <- get_eligible_stocks(file_paths, min_days, min_price, max_price, min_n_obs)
    universe_by_year[[as.character(as.integer(yr) + 1)]] <- eligible
  }
}

# Save universe
saveRDS(universe_by_year, "data/clean/stock_universe_by_year.rds")


# Build master set ---------------------------------------
df_all <- map_dfr(rds_files, function(file) {
  date_str <- extract_date(file)
  year_str <- extract_year(date_str)
  
  if (!(year_str %in% names(universe_by_year))) return(NULL)
  
  df <- readRDS(file)
  
  df_filtered <- df %>%
    filter(sym_root %in% universe_by_year[[year_str]]) %>%
    mutate(date = as.Date(date_str))
  
  return(df_filtered)
})

# Save the big file
saveRDS(df_all, "data/clean/data_cleaned.rds")

