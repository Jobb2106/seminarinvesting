# This script can be used to perform the FamaMacbeth regression. 

# Import packages ---------------------------------------------------------
library(tidyfinance)
library(lubridate)
library(dplyr)
library(tidyverse)
library(broom)


# Preparing the data ------------------------------------------------------
# Manually import the cor file
Fama_Macbeth <- Cor
# Fama_Macbeth <- readRDS("~/seminarininvesting/data/Corr.rds")

# Transform week column into right format for the estimate_fama-macbeth function
Fama_Macbeth$week <- ISOweek::ISOweek2date(paste0(Fama_Macbeth$week, "-1"))
Fama_Macbeth <- Fama_Macbeth %>% rename(date = week)
Fama_Macbeth <- na.omit(Fama_Macbeth) # Drop NA values

# Turn all returns into bps 
Fama_Macbeth$lagged_return <- 10000 * Fama_Macbeth$lagged_return
Fama_Macbeth$next_week_return <- 10000 * Fama_Macbeth$next_week_return


# One variable regressions  -----------------------------------------------
# Create a vector with all regressors
independent_vars <- c("jr_neg", "RSJ_week", "beta_daily", 
                      "log_market_cap", "bm", "lagged_return", "RES_week")

# Empty list to store models
results <- list()

# Loop through each independent variable and run regression
for (var in independent_vars) {
  formula_str <- paste("next_week_return ~", var)
  
  results[[var]] <- estimate_fama_macbeth(
    data = Fama_Macbeth,
    model = formula_str,
    vcov = "newey-west"
  )
}

# Create tidy summary table
tidy_results <- list()

summary_table <- purrr::map_dfr(independent_vars, function(var) {
  res <- results[[var]]
  
  if (!is.null(res) && is.data.frame(res)) {
    res %>%
      mutate(
        variable = var,
        risk_premium = formatC(risk_premium, format = "f", digits = 2),
        standard_error = formatC(standard_error, format = "f", digits = 2),
        t_statistic = formatC(t_statistic, format = "f", digits = 2)
      ) %>%
      select(variable, everything())
  } else {
    NULL
  }
})

# View the summary table
print(summary_table)


# Full model regression ---------------------------------------------------
# Run the full model with all variables
full_model <- estimate_fama_macbeth(
  data = Fama_Macbeth,
  model = "next_week_return ~ RSJ_week + jr_neg + beta_daily + log_market_cap + bm + lagged_return + RES_week",
  vcov = "newey-west"
)

# Create a summary table for the multiple regression
multiple_summary <- full_model %>%
  filter(factor != "intercept") %>%
  mutate(
    risk_premium = formatC(risk_premium, format = "f", digits = 2),
    t_statistic = formatC(t_statistic, format = "f", digits = 2)
  ) %>%
  select(factor, risk_premium, t_statistic)

# Show the result
print(multiple_summary)


# Regression with RSJ + firm-specific variables ---------------------------
model_RSJ_4F <- estimate_fama_macbeth(
  data = Fama_Macbeth,
  model = "next_week_return ~ RSJ_week + beta_daily + log_market_cap + bm + lagged_return",
  vcov = "newey-west"
)

summary_RSJ_4F <- model_RSJ_4F %>%
  filter(factor != "intercept") %>%
  mutate(
    risk_premium = formatC(risk_premium, format = "f", digits = 2),
    t_statistic  = formatC(t_statistic,  format = "f", digits = 2)
  ) %>%
  select(factor, risk_premium, t_statistic)

print(summary_RSJ_4F)


# Regression with JR + firm-specific variables ----------------------------
model_JR_4F <- estimate_fama_macbeth(
  data = Fama_Macbeth,
  model = "next_week_return ~ jr_neg + beta_daily + log_market_cap + bm + lagged_return",
  vcov = "newey-west"
)

summary_JR_4F <- model_JR_4F %>%
  filter(factor != "intercept") %>%
  mutate(
    risk_premium = formatC(risk_premium, format = "f", digits = 2),
    t_statistic  = formatC(t_statistic,  format = "f", digits = 2)
  ) %>%
  select(factor, risk_premium, t_statistic)

print(summary_JR_4F)


# Regression with RES + firm-specific variables ---------------------------
model_RES_4F <- estimate_fama_macbeth(
  data = Fama_Macbeth,
  model = "next_week_return ~ RES_week + beta_daily + log_market_cap + bm + lagged_return",
  vcov = "newey-west"
)

summary_RES_4F <- model_RES_4F %>%
  filter(factor != "intercept") %>%
  mutate(
    risk_premium = formatC(risk_premium, format = "f", digits = 2),
    t_statistic  = formatC(t_statistic,  format = "f", digits = 2)
  ) %>%
  select(factor, risk_premium, t_statistic)

print(summary_RES_4F)


# Regression with RSJ + JR + firm-specific variables ----------------------
model_RSJ_JR_4F <- estimate_fama_macbeth(
  data = Fama_Macbeth,
  model = "next_week_return ~ RSJ_week + jr_neg + beta_daily + log_market_cap + bm + lagged_return",
  vcov = "newey-west"
)

summary_RSJ_JR_4F <- model_RSJ_JR_4F %>%
  filter(factor != "intercept") %>%
  mutate(
    risk_premium = formatC(risk_premium, format = "f", digits = 2),
    t_statistic  = formatC(t_statistic,  format = "f", digits = 2)
  ) %>%
  select(factor, risk_premium, t_statistic)

print(summary_RSJ_JR_4F)


# Regression with RSJ + RES + firm-specific variables ---------------------
model_RSJ_RES_4F <- estimate_fama_macbeth(
  data = Fama_Macbeth,
  model = "next_week_return ~ RSJ_week + RES_week + beta_daily + log_market_cap + bm + lagged_return",
  vcov = "newey-west"
)

summary_RSJ_RES_4F <- model_RSJ_RES_4F %>%
  filter(factor != "intercept") %>%
  mutate(
    risk_premium = formatC(risk_premium, format = "f", digits = 2),
    t_statistic  = formatC(t_statistic,  format = "f", digits = 2)
  ) %>%
  select(factor, risk_premium, t_statistic)

print(summary_RSJ_RES_4F)


# Regression with JR + RES + firm-specific variables ----------------------
model_JR_RES_4F <- estimate_fama_macbeth(
  data = Fama_Macbeth,
  model = "next_week_return ~ jr_neg + RES_week + beta_daily + log_market_cap + bm + lagged_return",
  vcov = "newey-west"
)

summary_JR_RES_4F <- model_JR_RES_4F %>%
  filter(factor != "intercept") %>%
  mutate(
    risk_premium = formatC(risk_premium, format = "f", digits = 2),
    t_statistic  = formatC(t_statistic,  format = "f", digits = 2)
  ) %>%
  select(factor, risk_premium, t_statistic)

print(summary_JR_RES_4F)


# Regression with RSJ + JR + RES only -------------------------------------
model_RSJ_JR_RES <- estimate_fama_macbeth(
  data = Fama_Macbeth,
  model = "next_week_return ~ RSJ_week + jr_neg + RES_week",
  vcov = "newey-west"
)

summary_RSJ_JR_RES <- model_RSJ_JR_RES %>%
  filter(factor != "intercept") %>%
  mutate(
    risk_premium = formatC(risk_premium, format = "f", digits = 2),
    t_statistic  = formatC(t_statistic,  format = "f", digits = 2)
  ) %>%
  select(factor, risk_premium, t_statistic)

print(summary_RSJ_JR_RES)


# Regression with only firm-specific variables (no RSJ, JR, RES) ----------
model_4F_only <- estimate_fama_macbeth(
  data = Fama_Macbeth,
  model = "next_week_return ~ beta_daily + log_market_cap + bm + lagged_return",
  vcov = "newey-west"
)

summary_4F_only <- model_4F_only %>%
  filter(factor != "intercept") %>%
  mutate(
    risk_premium = formatC(risk_premium, format = "f", digits = 2),
    t_statistic  = formatC(t_statistic,  format = "f", digits = 2)
  ) %>%
  select(factor, risk_premium, t_statistic)

print(summary_4F_only)

