# This script is for the programming of the decomposition of the RV

# Data --------------------------------------------------------------------
# Laad de data uit GitHub
file_paths <- list.files("data/subset", pattern = "^filtered_\\d{4}-W\\d{2}\\.rds$", full.names = TRUE)

# dropped paths
dropped_files <- list.files("data/setdifference", pattern = "^dropped_data_\\d{4}-W\\d{2}\\.rds$", full.names = TRUE)

# FFC4 factors
ffc4_factors <- readRDS("data/metrics/FFC4.rds") %>%
  mutate(key = as.character(key))

# Market Cap data
market_cap <- readRDS("data/metrics/MarketCap.rds")


# Libraries ---------------------------------------------------------------
library(dplyr)
library(stringr)
library(lmtest)
library(sandwich)
library(purrr)
library(broom)
library(data.table)
library(ISOweek)
library(tidyr)

# RSJ/RES calculations -----------------------------------
# Functions for RV calculations
rv_negative <- function(returns) {
  neg_returns <- returns[returns < 0]
  sum(neg_returns^2)
}

rv_positive <- function(returns) {
  pos_returns <- returns[returns > 0]
  sum(pos_returns^2)
}

calculate_RSJ_day <- function(df) {
  df$rv_neg <- sapply(df$returns_5m, rv_negative)
  df$rv_pos <- sapply(df$returns_5m, rv_positive)
  
  df %>%
    mutate(
      signed_jump = rv_pos - rv_neg,
      RSJ_day = signed_jump / (rv_pos + rv_neg),
      date = as.Date(date)
    )
}

calculate_RES_day <- function(df, p = 0.05, scaling_factor = 78^0.5) {
  df %>%
    rowwise() %>%  # Process each row individually
    mutate(
      ES_p = {
        # Unlist the 5-minute returns for this row
        r_5m <- unlist(returns_5m)
        # Compute the quantile at probability p
        q_p <- quantile(r_5m, probs = p, na.rm = TRUE)
        # Compute the mean of returns below or equal to the quantile
        mean(r_5m[r_5m <= q_p], na.rm = TRUE)
      },
      RES = scaling_factor * ES_p
    ) %>%
    ungroup()
}

summarise_week <- function(df, week_id) {
  has_rsj <- "RSJ_day" %in% names(df)
  has_res <- "RES" %in% names(df)
  
  df %>%
    group_by(permno) %>%
    summarise(
      RSJ_week = if (has_rsj) mean(RSJ_day, na.rm = TRUE) else NA_real_,
      returns_week = sum(open_close_log_ret, na.rm = TRUE),
      RES_week = if (has_res) sqrt(n()) * sum(RES, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    ) %>%
    mutate(week_id = week_id)
}

# Calculations ------------------------------------------------------------
weekly_results <- list()
dropped_results <- list()

for (file in file_paths) {
  df <- readRDS(file)
  week_id <- str_remove(basename(file), "\\.rds$")
  clean_week_id <- str_remove(week_id, "^filtered_")
  
  # Normal calculations: first compute RSJ_day and RES
  df <- calculate_RSJ_day(df)
  df <- calculate_RES_day(df)  
  
  # Summarize weekly values (this aggregates RSJ_day, returns, and RES into weekly data)
  df_week_summary <- summarise_week(df, week_id)
  df_week_summary <- df_week_summary %>% filter(!is.na(RSJ_week))
  
  # Assign portfolios for both RSJ and RES sorts:
  df_week_summary <- df_week_summary %>%
    mutate(
      RSJ_portfolio = assign_portfolio(., sorting_variable = "RSJ_week", n_portfolios = 5),
      RES_portfolio = assign_portfolio(., sorting_variable = "RES_week", n_portfolios = 5)
    )
  
  weekly_results[[week_id]] <- df_week_summary
  
  dropped_file <- file.path("data/setdifference", paste0("dropped_data_", clean_week_id, ".rds"))
  # Process dropped data if exists
  if (file.exists(dropped_file)) {
    dropped_df <- readRDS(dropped_file)
    if (!"returns_week" %in% colnames(dropped_df)) {
      # dropped_df <- calculate_RSJ_day(dropped_df)
      # dropped_df <- calculate_RES_day(dropped_df)  # <-- add this call
      dropped_df <- summarise_week(dropped_df, clean_week_id)
    }
    dropped_results[[clean_week_id]] <- dropped_df
  }
}

# Portfolio Sorting -------------------------------------------------------
portfolio_performance <- list()
all_joined <- list()
week_ids <- sort(names(weekly_results))

for (i in 1:(length(week_ids) - 1)) {
  current_week_id <- week_ids[i]
  next_week_id <- week_ids[i + 1]
  clean_next_id <- sub("^filtered_", "", next_week_id)
  
  current_week <- weekly_results[[current_week_id]]
  next_week <- weekly_results[[next_week_id]]
  dropped_next <- dropped_results[[clean_next_id]]
  
  # Calculate next week returns
  joined <- add_next_week_return(
    current_week_df = current_week,
    next_week_df = next_week,
    dropped_df = dropped_next
  )
  
  # Filter out rows without next_week_return
  joined <- joined[!is.na(joined$next_week_return), ]
  
  # Create a clean week ID (e.g., "2025-W14")
  clean_week_date <- str_remove(current_week_id, "^filtered_")
  joined$week_id <- clean_week_date
  
  # Rolling join with market_cap using a temporary date column
  # Convert joined to data.table and add a temporary column "week_date" using ISOweek2date.
  joined_dt <- as.data.table(joined)
  market_cap_dt <- as.data.table(market_cap)
  
  # Create a temporary date column in joined_dt from week_id.
  # Here we use "-2" (Tuesday) or any weekday that makes sense; adjust as needed.
  joined_dt[, week_date := ISOweek2date(paste0(week_id, "-2"))]
  
  # Ensure market_cap_dt's date is a proper Date object.
  market_cap_dt[, date := as.Date(date, format = "%Y-%m-%d")]
  
  # Set keys: joined_dt on permno and week_date, and market_cap_dt on permno and date.
  setkey(joined_dt, permno, week_date)
  setkey(market_cap_dt, permno, date)
  
  # Perform the rolling join: if there is no exact match, use the last available market cap.
  joined_dt <- market_cap_dt[joined_dt, roll = TRUE]
  
  # Convert back to tibble and restore the clean week_id, then remove the temporary column.
  joined <- as_tibble(joined_dt) %>% 
    mutate(week_id = clean_week_date) %>% 
    select(-week_date)
  
  # Save the joined data (which now includes market cap) into all_joined.
  all_joined[[current_week_id]] <- joined
  
  # Calculate the equally weighted performance
  perf_eq <- summarise_portfolios(joined) %>%
    mutate(type = "equal", week_id = clean_week_date)
  
  # Calculate the value weighted performance
  perf_vw <- summarise_portfolios_value_weighted(joined) %>%
    mutate(type = "value", week_id = clean_week_date)
  
  # Combine equally and value weighted results for this week
  perf <- bind_rows(perf_eq, perf_vw)
  portfolio_performance[[current_week_id]] <- perf
}



# T5-T1, t value ----------------------------------------------------------
# Equally Weighted: Calculate the weekly spread for each sort type
performance_panel_equal <- bind_rows(portfolio_performance) %>%
  filter(type == "equal") %>%
  group_by(week_id, sort_type) %>%
  summarise(
    spread = first(avg_return[portfolio == max(portfolio)]) - 
      first(avg_return[portfolio == min(portfolio)]),
    .groups = "drop"
  )

# Separate the RSJ and RES results:
performance_panel_equal_rsj <- performance_panel_equal %>%
  filter(sort_type == "RSJ_portfolio")
performance_panel_equal_res <- performance_panel_equal %>%
  filter(sort_type == "RES_portfolio")

# Run regressions for each
model_equal_rsj <- lm(spread ~ 1, data = performance_panel_equal_rsj)
model_equal_res <- lm(spread ~ 1, data = performance_panel_equal_res)

cat("Equally weighted RSJ spread:\n")
print(coeftest(model_equal_rsj, vcov = NeweyWest))

cat("Equally weighted RES spread:\n")
print(coeftest(model_equal_res, vcov = NeweyWest))

# Value Weighted: Do the same for value weighted performance
performance_panel_value <- bind_rows(portfolio_performance) %>%
  filter(type == "value") %>%
  group_by(week_id, sort_type) %>%
  summarise(
    spread = first(avg_return[portfolio == max(portfolio)]) - 
      first(avg_return[portfolio == min(portfolio)]),
    .groups = "drop"
  )

performance_panel_value_rsj <- performance_panel_value %>%
  filter(sort_type == "RSJ_portfolio")
performance_panel_value_res <- performance_panel_value %>%
  filter(sort_type == "RES_portfolio")

model_value_rsj <- lm(spread ~ 1, data = performance_panel_value_rsj)
model_value_res <- lm(spread ~ 1, data = performance_panel_value_res)

cat("Value weighted RSJ spread:\n")
print(coeftest(model_value_rsj, vcov = NeweyWest))

cat("Value weighted RES spread:\n")
print(coeftest(model_value_res, vcov = NeweyWest))



# Portfolio Summary -------------------------------------------------------
# Combined portfolio performance from all weeks:
combined_df <- bind_rows(portfolio_performance)

# Equally weighted portfolio summary:
portfolio_summary_equal <- combined_df %>%
  filter(type == "equal") %>%
  group_by(sort_type, portfolio) %>%
  summarise(
    mean_return = mean(avg_return, na.rm = TRUE),
    .groups = "drop"
  )

# Value weighted portfolio summary:
portfolio_summary_value <- combined_df %>%
  filter(type == "value") %>%
  group_by(sort_type, portfolio) %>%
  summarise(
    mean_return = mean(avg_return, na.rm = TRUE),
    .groups = "drop"
  )


# FFC4 --------------------------------------------------------------------
# Equal weighted
portfolio_returns_weekly_equal <- combined_df %>%
  filter(type == "equal") %>%
  group_by(week_id, sort_type, portfolio) %>%
  summarise(avg_log_return = mean(avg_return, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(clean_week_id = str_remove(week_id, "^filtered_"))

returns_with_factors_equal <- portfolio_returns_weekly_equal %>%
  left_join(ffc4_factors, by = c("clean_week_id" = "key"))

ffc4_alpha_results_equal <- returns_with_factors_equal %>%
  group_by(sort_type, portfolio) %>%
  group_modify(~{
    model <- lm(avg_log_return ~ mkt_excess + smb + hml + mom, data = .x)
    tidy(model, conf.int = TRUE, conf.level = 0.95) %>%
      filter(term == "(Intercept)") %>%
      select(estimate, std.error, statistic)
  })

# Value weighted variant:
portfolio_returns_weekly_value <- combined_df %>%
  filter(type == "value") %>%
  group_by(week_id, sort_type, portfolio) %>%
  summarise(avg_log_return = mean(avg_return, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(clean_week_id = str_remove(week_id, "^filtered_"))

returns_with_factors_value <- portfolio_returns_weekly_value %>%
  left_join(ffc4_factors, by = c("clean_week_id" = "key"))

ffc4_alpha_results_value <- returns_with_factors_value %>%
  group_by(sort_type, portfolio) %>%
  group_modify(~{
    model <- lm(avg_log_return ~ mkt_excess + smb + hml + mom, data = .x)
    tidy(model, conf.int = TRUE, conf.level = 0.95) %>%
      filter(term == "(Intercept)") %>%
      select(estimate, std.error, statistic)
  })

