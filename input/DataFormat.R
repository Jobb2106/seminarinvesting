# Change format of the date column for easy sorting

# Import packages ---------------------------------------------------------
library(dplyr)
library(stringr)


# Changing format ---------------------------------------------------------
# Folder containing the original RDS files
rds_folder <- "/Users/job/Desktop/RDS3"
cleaned_folder <- "input/cleanedsubset"

# List all .rds files
rds_files <- list.files(rds_folder, pattern = "\\.rds$", full.names = TRUE)

# Loop through and clean with progress
total_files <- length(rds_files)

# Loop through and clean
for (i in seq_along(rds_files)) {
  file <- rds_files[i]
  file_name <- basename(file)
  
  df <- tryCatch(readRDS(file), error = function(e) {
    cat("Error reading", file_name, ":", conditionMessage(e), "\n")
    return(NULL)
  })
  
  if (!is.null(df)) {
    if ("date" %in% colnames(df)) {
      df <- df %>%
        mutate(date = as.character(date)) %>%
        mutate(
          date = if_else(
            str_detect(date, "^\\d{8}$"),
            str_c(
              substr(date, 1, 4), "-",
              substr(date, 5, 6), "-",
              substr(date, 7, 8)
            ),
            date
          )
        )
    }
    
    # Save cleaned file
    out_path <- file.path(cleaned_folder, file_name)
    saveRDS(df, out_path)
    
    # Show progress
    cat(sprintf("[%d/%d] Cleaned: %s\n", i, total_files, file_name))
  }
}
