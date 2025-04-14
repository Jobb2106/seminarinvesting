# Compute correlation matrix and summary stats

# Import packages ---------------------------------------------------------
library(tidyverse)
library(RSQLite)
library(scales)
library(lmtest)
library(broom)
library(sandwich)

#results_book[, lagged_date := date + weeks(1)]
#results_book[, lagged_week := week_key(lagged_date)]

setorder(results_book, permno, date)  # Ensure it's sorted properly

results_book[, lagged_return := shift(next_week_return, type = "lag"), by = permno]



# Create df with all variables (matched) ---------------------------------------------------------------
weekly_all_corr <- bind_rows(results_book) %>% 
  mutate(
    week = as.character(week),
    log_market_cap = log(market_cap)
  ) %>%
  select(week, permno, RSJ_week, jr_neg, log_market_cap, lagged_return, beta_daily, bm, RES_week) %>%
  filter(!is.na(log_market_cap))

saveRDS(weekly_all_corr, "data/Corr.rds")

# Compute correlations ----------------------------------------------------
# Define the variables to correlate.
vars <- c("RSJ_week", "jr_neg", "log_market_cap", "lagged_return", "beta_daily", "bm", "RES_week")

corr_list <- weekly_all_corr %>%
  group_by(week) %>%
  group_map(~ {
    # Select only the columns of interest and compute correlation
    cor(select(.x, all_of(vars)), use = "pairwise.complete.obs")
  })

n_weeks <- length(corr_list)
corr_array <- array(unlist(corr_list), dim = c(length(vars), length(vars), n_weeks))

# Compute the mean correlation matrix by applying mean over the third dimension
mean_corr <- apply(corr_array, c(1, 2), mean, na.rm = TRUE)

# Assign row and column names for clarity
rownames(mean_corr) <- vars
colnames(mean_corr) <- vars

# Print the final mean correlation matrix
print(mean_corr)


# Compute mean and standard Error for the variables -----------------------
overall_stats <- data.frame(
  Variable = vars,
  Mean = sapply(vars, function(v) mean(weekly_all_corr[[v]], na.rm = TRUE)),
  StdError = sapply(vars, function(v) sd(weekly_all_corr[[v]], na.rm = TRUE) / sqrt(sum(!is.na(weekly_all_corr[[v]]))))
)

cat("\nOverall mean and standard error computed from all observations:\n")
print(overall_stats)


