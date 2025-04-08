library(dplyr)
library(purrr)
library(tidyverse)
library(stringr)
library(readr)

rds_folder <- "/Users/job/Desktop/RDS"
#rds_folder <- "/Users/thorhogerbrugge/Desktop/RDS"
rds_files <- list.files(rds_folder, pattern = "\\.rds$", full.names = TRUE)

safe_read <- function(path) {
  tryCatch(readRDS(path), error = function(e) {
    cat("Skipping file:", path, "due to error:", conditionMessage(e), "\n")
    return(NULL)
  })
}

safe_read(rds_files)

filename <- file.choose()
test <- readRDS(filename)


df_debug <- `filtered_2013-W07-v6`

df_debug %>%
  group_by(permno) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  print(n = Inf)



# Load both versions
v1 <- `filtered_2013-W07-v6`
v2 <- `filtered_2013-W07`

# Get unique stock identifiers (use either permno or sym_root)
stocks_v1 <- unique(v1$permno)
stocks_v2 <- unique(v2$permno)

# Compare
only_in_v1 <- setdiff(stocks_v1, stocks_v2)
only_in_v2 <- setdiff(stocks_v2, stocks_v1)

# Pad shorter vector with NAs to match lengths
max_len <- max(length(only_in_v1), length(only_in_v2))
only_in_v1 <- c(only_in_v1, rep(NA, max_len - length(only_in_v1)))
only_in_v2 <- c(only_in_v2, rep(NA, max_len - length(only_in_v2)))

# Combine
difference_df <- tibble(
  only_in_v1 = only_in_v1,
  only_in_v2 = only_in_v2
)

print(difference_df)







# Code opslaan ------------------------------------------------------------
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
  
  valid_rows <- df_week %>%
    filter(
      !is.na(close_crsp),
      close_crsp >= min_price,
      close_crsp <= max_price,
      n_obs >= min_n_obs
    )
  
  valid_day_counts <- valid_rows %>%
    group_by(permno) %>%
    summarise(n_valid_days = n_distinct(date), .groups = "drop") %>%
    filter(n_valid_days >= required_days)
  
  df_filtered <- valid_rows %>%
    filter(permno %in% valid_day_counts$permno)

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
