library(dplyr)
library(stringr)

# Folder containing the original RDS files
rds_folder <- "/Users/job/Desktop/RDS3"
cleaned_folder <- "input/cleaned"

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
      df$date <- as.character(df$date)  # Coerce to character
    }
    
    # Save cleaned file
    out_path <- file.path(cleaned_folder, file_name)
    saveRDS(df, out_path)
    
    # Show progress
    cat(sprintf("[%d/%d] Cleaned: %s\n", i, total_files, file_name))
  }
}
