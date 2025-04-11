
# Create df
weekly_all_corr <- bind_rows(results) %>% 
  mutate(
    week = as.character(week),
    log_market_cap = log(market_cap)
  ) %>%
  select(week, permno, RSJ_week, jr_neg, log_market_cap, next_week_return) %>%
  filter(!is.na(log_market_cap))

# Define the variables to correlate.
vars <- c("RSJ_week", "jr_neg", "log_market_cap", "next_week_return")

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