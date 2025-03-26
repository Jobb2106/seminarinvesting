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
library(tidyr)
library(dplyr)

# Portfolio Sorting -------------------------------------------------------
# We sort based on RSJ on Tuesdays
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

# Create a version where we lead the return (i.e., return at T+1 is linked to portfolio at T)
weekly_df <- rsj_portfolios %>%
  arrange(permno, date) %>%
  group_by(permno) %>%
  mutate(returns_next_week = lead(returns_week)) %>%
  ungroup()

# Average return per portfolio per date
# Note dat we hier dus nog voor elke datum gemiddelde moeten berekenen
portfolio_returns <- weekly_df %>%
  filter(!is.na(returns_next_week)) %>%
  group_by(date, portfolio) %>%
  summarize(
    mean_return = mean(returns_next_week, na.rm = TRUE),
    .groups = "drop"
  )


### Step 2: High-low spread (T5-T1)
long_short_df <- portfolio_returns %>%
  pivot_wider(names_from = portfolio, values_from = mean_return, names_prefix = "P") %>%
  mutate(
    long_short = P5 - P1
  )

# Newey-West standard errors
# Run a regression: long_short ~ 1 (to get mean and t-stat)
nw_model <- lm(long_short ~ 1, data = long_short_df)

nw_tstat <- coeftest(nw_model, vcov = NeweyWest(nw_model, lag = 4))



### Step 3: Calculate FFC4 alpha
# Moeten deze data nog ergens vandaan toveren, die code zou niet heel lastig moeten zijn


