# This script can be used for portfolio sorting
# Deze link van TidyVerse heb ik vooral gebruikt: https://www.tidy-finance.org/r/univariate-portfolio-sorts.html


# Data --------------------------------------------------------------------
# Laad de data uit GitHub
df <- readRDS("data/clean/returns_5m_all_subset.rds")

# test, leave out NA for first obs
df <- df %>%
  filter(as.Date(date) != as.Date("2016-11-01"))


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(RSQLite)
library(scales)
library(lmtest)
library(broom)
library(sandwich)

# Portfolio Sorting -------------------------------------------------------
# We sort based on RSJ on tuesdays
assign_portfolios <- function(data, sorting_variable, n_portfolios = 5) {
  data %>%
    group_by(date) %>%
    mutate(
      # Breakpoints per Tuesday
      breakpoint = list(quantile(
        {{ sorting_variable }},
        probs = seq(0, 1, length.out = n_portfolios + 1),
        na.rm = TRUE,
        names = FALSE
      )),
      portfolio = findInterval(
        {{ sorting_variable }},
        breakpoint[[1]],
        all.inside = TRUE
      )
    ) %>%
    ungroup() %>%
    select(-breakpoint)
}

# Filter Tuesdays only
df_tuesday <- df %>%
  filter(weekdays(date) == "Tuesday")

# Assign portfolios using RSJ_week
rsj_portfolios <- assign_portfolios(
  data = df_tuesday,
  sorting_variable = RSJ_week,
  n_portfolios = 5
) %>%
  select(permno, date, portfolio, RSJ_week, returns_week)

# Sanity check
hist(rsj_portfolios$portfolio)
sum(is.na(rsj_portfolios$portfolio))


# Performance Evaluation --------------------------------------------------

### Step 1: Average Return for upcoming week
# op dit moment staat in rsj_portfolios de returns voor die week, dus laggen
# werkt pas als we met grotere subsample gaan werken

weekly_df <- rsj_portfolios %>%
  arrange(permno, date) %>%
  group_by(permno) %>%
  mutate(portfolio_lag = lag(portfolio)) %>%
  ungroup()

portfolio_returns <- weekly_df %>%
  filter(!is.na(portfolio_lag)) %>%
  group_by(date, portfolio_lag) %>%
  summarize(
    mean_returns = mean(returns_week, na.rm = TRUE),
    .groups = "drop"
  )


### Step 2: High-low spread
#TOCHECK



