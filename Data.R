# This is where the data magic happens

library(dplyr)
library(readr)
library(tidyverse)

# Set folders -------------------------------------------------------------
rds_folder <- "/Users/job/Desktop/RDS"
# rds_folder <- "/Users/thorhogerbrugge/Desktop/RDS"
rds_files <- list.files(rds_folder, pattern = "\\.rds$", full.names = TRUE)

rds_files_test <- rds_files[6000:6005]


# Extract date -------------------------------------------------------------------
extract_date <- function(path) {
  str_extract(basename(path), "\\d{4}-\\d{2}-\\d{2}")
}


# Build master set ---------------------------------------
# Hier maken we 1 grote dataframe van de data zodat we mooie dingen kunnen gaan doen
df_all <- map_dfr(rds_files_test, function(file) {
  date <- extract_date(file)
  df <- readRDS(file)
  
  # TOCHECK
  df %>%
    filter(close_crsp > 5, close_crsp < 1000) %>%
    mutate(date = date)
})

# subset (voor het programmeren)
saveRDS(df_all, "data/clean/returns_5m_all_subset.rds")

# big file (voor de uiteindelijke resultaten)
saveRDS(df_all, "data/clean/returns_5m_all.rds")


# Template measures -------------------------------------------------------
# Voorlopig niks mee doen
df <- readRDS("data/clean/returns_5m_all.rds")

df_metrics <- df %>%
  mutate(
    rv = map_dbl(returns_5m, ~ sum((.x)^2, na.rm = TRUE)),
    es_95 = map_dbl(returns_5m, ~ mean(.x[.x < quantile(.x, 0.05, na.rm = TRUE)], na.rm = TRUE))
  ) %>%
  select(sym_root, date, close_crsp, rv, es_95)

saveRDS(df_metrics, "data/metrics/metrics.rds")
