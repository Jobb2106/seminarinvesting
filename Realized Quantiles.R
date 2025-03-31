# This script is used to compute the realized quantiles

# Packages ----------------------------------------------------------------
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)


# Import data --------------------------------------------------------------------
file_paths <- list.files("data/subset", pattern = "^filtered_\\d{4}-W\\d{2}\\.rds$", full.names = TRUE)
output_folder <- "data/weekly_filtered_RQ"
#if (!dir.exists(output_folder)) dir.create(output_folder)


# Realized quantiles parameters ------------------------------------------------------
p <- 0.05
c <- 78
H <- 0.5
#scaling_factor <- 1
scaling_factor <- c^H


# Calculate Quantile and RES ----------------------------------------------------------------
process_with_rq <- function(file_path) {
  df <- readRDS(file_path)
  
  #long format
  df_long <- df %>%
    select(sym_root, date, returns_5m) %>%
    unnest_longer(returns_5m, values_to = "r_5m") %>%
    filter(!is.na(r_5m))
  
  #Compute daily quantile and ES
  rq_data <- df_long %>%
    group_by(sym_root, date) %>%
    summarise(
      Q_p = quantile(r_5m, probs = p, na.rm = TRUE),
      ES_p = mean(r_5m[r_5m <= quantile(r_5m, probs = p, na.rm = TRUE)], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      RQ = scaling_factor * Q_p,
      RES = scaling_factor * ES_p
    )
  
  #Join back into the original df by date
  df <- df %>%
    left_join(rq_data %>% select(sym_root, date, RQ, RES), by = c("sym_root", "date"))
  
  week_id <- str_extract(basename(file_path), "20\\d{2}-W\\d{2}")
  saveRDS(df, file.path(output_folder, paste0("rsj_rq_", week_id, ".rds")))
  
  cat("Saved:", week_id, "\n")
  return(NULL)
}


# Process all files -------------------------------------------------------
walk(file_paths, process_with_rq)


# (Double) sorten & Fama-French --------------------------------------------
# Hierna dan (double) sorten & opnieuw Fama-French regressie

