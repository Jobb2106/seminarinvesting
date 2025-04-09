# This script is for the programming of the decomposition of the RV

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

# Data --------------------------------------------------------------------
# Laad de data uit GitHub
rds_folder <- "/Users/thorhogerbrugge/Desktop/Filtered"
file_paths <- list.files(rds_folder, pattern = "^filtered_\\d{4}-W\\d{2}\\.rds$", full.names = TRUE)

# dropped paths
dropped_files <- list.files("/Users/thorhogerbrugge/Desktop/Dropped", pattern = "^dropped_data_\\d{4}-W\\d{2}\\.rds$", full.names = TRUE)

# FFC4 factors
ffc4_factors <- readRDS("data/metrics/FFC4.rds") %>%
  mutate(key = as.character(key))

# Market Cap data
market_cap <- readRDS("data/metrics/MarketCap.rds")


# Calculation Functions -----------------------------------
# Functions for RV calculations
calculate_RSJ_day <- function(df) {
  df %>%
    mutate(
      rv_neg = sapply(returns_5m, function(r) sum(r[r < 0]^2)),
      rv_pos = sapply(returns_5m, function(r) sum(r[r > 0]^2)),
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
        # Compute the scaled returns below or equal to the quantile
        ES_p <- (1 / (78 * p)) * sum(r_5m[r_5m <= q_p])
      },
      RES = scaling_factor * ES_p
    ) %>%
    ungroup()
}

# Global parameter (mu should already be defined elsewhere)
mu <- sqrt(2/pi)

# JR negative weekly aggregation: calculate daily JR negative and aggregate by summing
calculate_JR_negative_week <- function(df, week_id) {
  df %>%
    mutate(
      rv_neg    = map_dbl(returns_5m, ~ sum((.x[.x < 0])^2)),
      bpv_neg   = map_dbl(returns_5m, ~ sum(abs(.x)[-1] *
                                              abs(.x)[-length(.x)] *
                                              (.x[-1] < 0))),
      jv_neg    = pmax(rv_neg - mu^(-2) * bpv_neg, 0),
      jr_neg    = jv_neg / rv_neg
    ) %>%
    filter(!is.na(jr_neg) & is.finite(jr_neg)) %>%
    group_by(permno) %>%
    summarise(jr_neg = sum(jr_neg), .groups = "drop") %>%  # or use mean() if desired
    mutate(week_id = week_id)
}


# Global parameters
v <- 0.6090; mu <- sqrt(2/pi); M <- 78; delta <- 1/M

# Helper functions
RV <- function(returns) sum(returns^2)

BPV <- function(returns) {
  sum(abs(returns[-1]) * abs(returns[-length(returns)]))
}

QPV <- function(returns) {
  abs_returns <- abs(returns)
  M * sum(
    abs_returns[1:(length(abs_returns)-3)] *
      abs_returns[2:(length(abs_returns)-2)] *
      abs_returns[3:(length(abs_returns)-1)] *
      abs_returns[4:length(abs_returns)]
  )
}

AJR <- function(RV, BPV, QPV, M, mu) {
  wortel <- max(1, QPV / (BPV^2))
  (sqrt(M) / sqrt(wortel)) * ((mu^(-2)) * BPV / RV - 1)
}

# Single function: Compute daily AJR and aggregate to weekly values (per stock)
calculate_AJR_week <- function(df, week_id) {
  df %>%
    mutate(
      rv     = map_dbl(returns_5m, RV),
      bpv    = map_dbl(returns_5m, BPV),
      qpv    = map_dbl(returns_5m, QPV),
      ajr_day= mapply(AJR, rv, bpv, qpv, MoreArgs = list(M = M, mu = mu))
    ) %>%
    filter(!is.na(ajr_day)) %>%
    group_by(permno) %>%
    summarise(
      ajr_sum  = sum(ajr_day, na.rm = TRUE),
      # ajr_mean = mean(ajr_day, na.rm = TRUE),
      n_days   = n(),
      .groups  = "drop"
    ) %>%
    mutate(
      ajr_median    = median(ajr_sum, na.rm = TRUE),
      AJR_portfolio = if_else(ajr_sum < ajr_median, "jump", "continuous"),
      week_id         = week_id
    ) %>%
    select(-ajr_median)
}

summarise_week <- function(df, week_id) {
  has_rsj <- "RSJ_day" %in% names(df)
  has_res <- "RES" %in% names(df)
  
  df %>%
    group_by(permno) %>%
    summarise(
      RSJ_week = if (has_rsj) mean(RSJ_day, na.rm = TRUE) else NA_real_,
      log_returns_week = sum(open_close_log_ret, na.rm = TRUE),
      simple_returns_week = prod(ret_crsp / 100, na.rm = TRUE) - 1,
      RES_week = if (has_res) sqrt(n()) * sum(RES, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    ) %>%
    mutate(week_id = week_id)
}
# ------------------------------------------------------------------------------
# Onderstaande functie hoeft niet meer gerunt te worden. Toch maar laten staan
# voor de zekerheid 
# ------------------------------------------------------------------------------

# add_next_week_return <- function(current_week_df, next_week_df, dropped_df) {
#   # Stap 1: Haal returns op uit next_week_df
#   next_returns <- next_week_df %>%
#     select(permno, returns_week) %>%
#     rename(next_week_return = returns_week)
#   
#   # Stap 2: Left join met de data van de huidige week
#   merged_df <- current_week_df %>%
#     left_join(next_returns, by = "permno")
#   
#   # Stap 3: Indien er returns ontbreken, vul in met data uit dropped_df
#   if (!is.null(dropped_df) && any(is.na(merged_df$next_week_return))) {
#     fallback_returns <- dropped_df %>%
#       select(permno, returns_week) %>%
#       rename(next_week_return = returns_week)
#     
#     merged_df <- merged_df %>%
#       left_join(fallback_returns, by = "permno", suffix = c("", ".fallback")) %>%
#       mutate(next_week_return = coalesce(next_week_return, next_week_return.fallback)) %>%
#       select(-next_week_return.fallback)
#   }
#   
#   return(merged_df)
# }



# Calculations ------------------------------------------------------------
# Deze shit hoeft dus maar 1x voor alles gerund en dan zijn we het baasje. Als alles in 1x te veel is kunnen we chuncks doen. 
# Als we alles een keertje narekenen voor de zekerheid is dit echt top
weekly_results <- list()
dropped_results <- list()
n_files <- length(file_paths)

for(i in 1:(n_files - 1)) {
  current_file <- file_paths[i]
  df_current <- readRDS(current_file)
  week_id <- str_remove(basename(current_file), "\\.rds$")
  clean_week_id <- str_remove(week_id, "^filtered_")
  
  # Normal calculations: compute RSJ_day and RES
  df_current <- calculate_RSJ_day(df_current)
  df_current <- calculate_RES_day(df_current)  
  
  # Summarize weekly values (aggregates RSJ_day, returns, and RES into weekly data)
  df_week_summary <- summarise_week(df_current, week_id) %>%
    filter(!is.na(RSJ_week))
  
  # Calculate the weekly AJR summary and negative jump ratio (JR negative)
  df_AJR_week <- calculate_AJR_week(df_current, week_id)
  df_JVneg_week <- calculate_JR_negative_week(df_current, week_id)
  
  # Merge summaries by stock identifier (ensure the common key is consistent)
  df_merged_current <- df_week_summary %>%
    full_join(df_AJR_week, by = c("permno", "week_id")) %>%  # if summarise_week() returns column "week"
    full_join(df_JVneg_week, by = c("permno", "week_id"))
  
  # Process next week data
  next_file <- subset_file_paths[i + 1]
  df_next <- readRDS(next_file)
  next_week_id <- str_remove(basename(next_file), "\\.rds$")
  clean_next_id <- str_remove(next_week_id, "^filtered_")
  
  next_week_summary <- summarise_week(df_next, next_week_id)
  
  # Process dropped data for next week (if exists)
  dropped_file <- file.path("/Users/thorhogerbrugge/Desktop/Dropped", paste0("dropped_data_", clean_next_id, ".rds"))
  if (file.exists(dropped_file)) {
    dropped_df <- readRDS(dropped_file)
    if (!"returns_week" %in% colnames(dropped_df)) {
      dropped_df <- summarise_week(dropped_df, clean_next_id)
    }
    dropped_next <- dropped_df
  } else {
    dropped_next <- NULL
  }
  
  # Add next week return using your add_next_week_return() helper
  joined <- add_next_week_return(
    current_week_df = df_merged_current,
    next_week_df    = next_week_summary,
    dropped_df      = dropped_next
  )
  
  # Filter out rows without next_week_return and add the week identifier
  joined <- joined %>%
    filter(!is.na(next_week_return)) %>%
    mutate(week = clean_week_id)
  
  # Market Cap Rolling Join using data.table
  joined_dt <- as.data.table(joined)
  market_cap_dt <- as.data.table(market_cap)
  
  # Create temporary date column: using Tuesday as an example (adjust "-2" if needed)
  joined_dt[, week_date := ISOweek2date(paste0(week, "-2"))]
  market_cap_dt[, date := as.Date(date, format = "%Y-%m-%d")]
  
  # Set keys and perform rolling join
  setkey(joined_dt, permno, week_date)
  setkey(market_cap_dt, permno, date)
  joined_dt <- market_cap_dt[joined_dt, roll = TRUE]
  
  # Store the joined data in the results list using the clean week id
  weekly_results[[clean_week_id]] <- joined_dt
}


# Portfolio Sorting -------------------------------------------------------




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

