# Template measures -------------------------------------------------------

# Folder with weekly filtered .rds files
filtered_folder <- "data/weekly_filtered"
filtered_files <- list.files(filtered_folder, pattern = "\\.rds$", full.names = TRUE)

# Functions to compute RV parts
rv_negative <- function(returns) {
  neg_returns <- returns[returns < 0]
  sum(neg_returns^2)
}