# Compute correlation matrix, summary stats and plot variable density

# Import packages ---------------------------------------------------------
library(tidyverse)
library(RSQLite)
library(scales)
library(lmtest)
library(broom)
library(sandwich)


# Create data frame ----------------------------------------------------------
all_results <- bind_rows(results) #use results

beta_daily <- betaresults #get betas

# add month_key column
all_results[, month_key := floor_date(date, unit = "month")]
setkey(all_results, permno, month_key)
setDT(beta_daily)

beta_daily[, month_key := floor_date(date, unit = "month")]
setkey(beta_daily, permno, month_key)

#merge
results_beta <- merge(
  x = all_results,
  y = beta_daily[, list(permno, month_key, beta_daily)],
  by = c("permno", "month_key"),
  all.x = TRUE
)

# Add book to market
setDT(booktomarket)

booktomarket[, lagged_date := date %m+% months(3)] #lag by 3 months for lookahead bias
booktomarket[, month_key := floor_date(lagged_date, unit = "month")]

setkey(booktomarket, permno, month_key)

results_book <- merge(
  x = results_beta,
  y = booktomarket[, list(permno, month_key, bm)],
  by = c("permno", "month_key"),
  all.x = TRUE
)

# Lag returns
setorder(results_book, permno, date)  # Ensure it's sorted properly
results_book[, lagged_return := shift(next_week_return, type = "lag"), by = permno] #lag returns by one week

#saveRDS(results_book, "data/ResultsBook.rds") #save dataframe


# Create df with all variables needed (matched) ---------------------------------------------------------------
weekly_all_corr <- bind_rows(results_book) %>% 
  mutate(
    week = as.character(week),
    log_market_cap = log(market_cap)
  ) %>%
  select(week, permno, RSJ_week, jr_neg, log_market_cap, lagged_return, beta_daily, bm, RES_week, next_week_return) %>%
  filter(!is.na(log_market_cap))

#saveRDS(weekly_all_corr, "/Users/job/Desktop/Cor.rds")


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
  Mean = sapply(vars, function(v) mean(results_book[[v]], na.rm = TRUE)),
  StdError = sapply(vars, function(v) sd(results_book[[v]], na.rm = TRUE))
)

print(overall_stats) # Print


# Calculate and plot density for each variable ------------------------------------------------------------
dens_RSJ <- density(Cor$RSJ_week)
dens_RES <- density(Cor$RES_week)
dens_JRneg <- density(Cor$jr_neg)

plot(dens_RSJ, main = "RSJ")
plot(dens_RES, main = "RES")
plot(dens_JRneg, main = "JR neg")


