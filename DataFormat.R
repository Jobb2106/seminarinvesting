library(dplyr)
library(stringr)

# Folder containing the original RDS files
rds_folder <- "/Users/job/Desktop/RDS3"
cleaned_folder <- "input/cleaned"

# List all .rds files
rds_files <- list.files(rds_folder, pattern = "\\.rds$", full.names = TRUE)

# Loop through and clean
for (file in rds_files) {
  df <- tryCatch(readRDS(file), error = function(e) {
    cat("Error reading", file, ":", conditionMessage(e), "\n")
    return(NULL)
  })
  
  if (!is.null(df)) {
    if ("date" %in% colnames(df)) {
      df$date <- as.character(df$date)  # Coerce to character
    }
    
    # Save cleaned file
    out_path <- file.path(cleaned_folder, basename(file))
    saveRDS(df, out_path)
  }
}
