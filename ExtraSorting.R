# Voor THOR: de code met de simple returns staat onderaan 

# Assumes weekly_results is a list of weekly data.frames
weekly_all <- bind_rows(weekly_results) %>% 
  mutate(
    week = as.character(week),
    returns_week = exp(returns_week / 100) - 1 # Convert log to arithmetic
  ) %>%
  select(week, permno, RSJ_week, RES_week, market_cap, returns_week)


# Function to assign portfolios 
assign_portfolio <- function(data, 
                             sorting_variable, 
                             n_portfolios) {
  # Compute breakpoints
  breakpoints <- data |>
    pull({{ sorting_variable }}) |>
    quantile(
      probs = seq(0, 1, length.out = n_portfolios + 1),
      na.rm = TRUE,
      names = FALSE
    )
  
  # Assign portfolios
  assigned_portfolios <- data |>
    mutate(portfolio = findInterval(
      pick(everything()) |>
        pull({{ sorting_variable }}),
      breakpoints,
      all.inside = TRUE
    )) |>
    pull(portfolio)
  
  # Output
  return(assigned_portfolios)
}



library(data.table)

# Assuming weekly_all is already a data.table
setorder(weekly_all, permno, week)  # sort by permno and week (important for shifting correctly)

# Add lagged RSJ_week by permno
weekly_all[, RSJ_lag := shift(RSJ_week, n = 1L, type = "lag"), by = permno] 

weekly_all <- na.omit(weekly_all)

RSJ_portfolios <- weekly_all |>
  group_by(week) |>
  mutate(
    portfolio = assign_portfolio(
      data = pick(everything()),
      sorting_variable = RSJ_lag,
      n_portfolios = 5
    ),
    portfolio = as.factor(portfolio)
  ) |>
  group_by(portfolio, week) |>
  summarize(
    ret_excess = mean(returns_week),
    .groups = "drop"
  )

# Equal-Weighted Portfolio Average Returns (RSJ)
rsj_ar_ew <- RSJ_portfolios %>%
  group_by(portfolio) %>%
  summarize(
    avg_return = mean(ret_excess, na.rm = TRUE),
    n_weeks = n(),
    .groups = "drop"
  )

# Simple returns ---------------------------------------------------------------

weekly_all <- bind_rows(weekly_results) %>% 
  mutate(
    week = as.character(week),
  ) %>%
  select(week, permno, RSJ_week, RES_week, market_cap, simple_returns_week) # Hier neem ik aan dat simple_returns_week erin zit, effe checken 

# Function to assign portfolios 
assign_portfolio <- function(data, 
                             sorting_variable, 
                             n_portfolios) {
  # Compute breakpoints
  breakpoints <- data |>
    pull({{ sorting_variable }}) |>
    quantile(
      probs = seq(0, 1, length.out = n_portfolios + 1),
      na.rm = TRUE,
      names = FALSE
    )
  
  # Assign portfolios
  assigned_portfolios <- data |>
    mutate(portfolio = findInterval(
      pick(everything()) |>
        pull({{ sorting_variable }}),
      breakpoints,
      all.inside = TRUE
    )) |>
    pull(portfolio)
  
  # Output
  return(assigned_portfolios)
}



library(data.table)

# Assuming weekly_all is already a data.table
setorder(weekly_all, permno, week)  # sort by permno and week (important for shifting correctly)

# Add lagged RSJ_week by permno
weekly_all[, RSJ_lag := shift(RSJ_week, n = 1L, type = "lag"), by = permno] 

weekly_all <- na.omit(weekly_all)

RSJ_portfolios <- weekly_all |>
  group_by(week) |>
  mutate(
    portfolio = assign_portfolio(
      data = pick(everything()),
      sorting_variable = RSJ_lag,
      n_portfolios = 5
    ),
    portfolio = as.factor(portfolio)
  ) |>
  group_by(portfolio, week) |>
  summarize(
    ret_excess = mean(simple_returns_week),
    .groups = "drop"
  )

# Equal-Weighted Portfolio Average Returns (RSJ)
rsj_ar_ew <- RSJ_portfolios %>%
  group_by(portfolio) %>%
  summarize(
    avg_return = mean(ret_excess, na.rm = TRUE),
    n_weeks = n(),
    .groups = "drop"
  )

