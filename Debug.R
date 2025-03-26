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