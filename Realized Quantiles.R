# This script is used to compute the realized quantiles
# TODO nog naar week

# Packages ----------------------------------------------------------------
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)


# Import data --------------------------------------------------------------------
file_paths <- list.files("data/subset", pattern = "^filtered_\\d{4}-W\\d{2}\\.rds$", full.names = TRUE)
output_folder <- "data/weekly_filtered_RQ"


# Parameters --------------------------------------------------------------
p <- 0.05
c <- 78
H <- 0.5
scaling_factor <- c^H

# Calculate Quantile and RES ----------------------------------------------------------------
process_with_rq_weekly <- function(file_path) {
  df <- readRDS(file_path)
  
  # Unnest 5-min returns
  df_long <- df %>%
    select(sym_root, date, returns_5m) %>%
    unnest_longer(returns_5m, values_to = "r_5m") %>%
    filter(!is.na(r_5m))
  
  # Compute daily ES
  daily_res <- df_long %>%
    group_by(sym_root, date) %>%
    summarise(
      ES_p = mean(r_5m[r_5m <= quantile(r_5m, probs = p, na.rm = TRUE)], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      RES = scaling_factor * ES_p,
      week_id = format(as.Date(date), "%Y-W%V")
    )
  
  # Compute RES_week = sqrt(n_days) * sum(RES)
  weekly_res <- daily_res %>%
    group_by(sym_root, week_id) %>%
    summarise(
      RES_week = sqrt(n()) * sum(RES, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Assign portfolios based on RES_week
  weekly_res$portfolio <- assign_portfolio(weekly_res, "RES_week", 5)
  
  # Return just what you need
  return(weekly_res)
}


# Process all files -------------------------------------------------------
walk(file_paths, process_with_rq)



# Performance  ------------------------------------------------------------
# Heb deze nu gekopieerd uit decomposition, maar willen we eigenlijk samen hebben in 1 loop
portfolio_performance <- list()
weekly_spreads <- list()
all_joined <- list()

# Sanity
week_ids <- sort(names(weekly_res))

for (i in 1:(length(week_ids) - 1)) {
  current_week_id <- week_ids[i]
  next_week_id <- week_ids[i + 1]
  clean_next_id <- sub("^filtered_", "", next_week_id)
  
  current_week <- weekly_results[[current_week_id]]
  next_week <- weekly_results[[next_week_id]]
  dropped_next <- dropped_results[[clean_next_id]]
  
  # Next week returns
  joined <- add_next_week_return(
    current_week_df = current_week,
    next_week_df = next_week,
    dropped_df = dropped_results[[clean_next_id]]
  )
  
  # >>> FILTER OUT NA values in next_week_return <<<
  joined <- joined[!is.na(joined$next_week_return), ]
  joined$week_id <- week_ids[i]
  all_joined[[week_ids[i]]] <- joined
  
  
  # Optional post-processing steps:
  perf <- summarise_portfolios(joined)
  perf$week_id <- week_ids[i]
  
  # Store result
  portfolio_performance[[week_ids[i]]] <- perf
}

# t-value for high-low spread
performance_panel <- bind_rows(portfolio_performance)
model <- lm(spread ~ 1, data = performance_panel)
coeftest(model, vcov = NeweyWest)


# FFC4 --------------------------------------------------------------------
portfolio_returns_weekly <- bind_rows(all_joined) %>%
  group_by(week_id, portfolio) %>%
  summarise(
    avg_log_return = mean(next_week_return, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(clean_week_id = str_remove(week_id, "^filtered_"))

returns_with_factors <- portfolio_returns_weekly %>%
  left_join(ffc4_factors, by = c("clean_week_id" = "key"))

ffc4_alpha_results <- returns_with_factors %>%
  group_by(portfolio) %>%
  group_modify(~{
    model <- lm(avg_log_return ~ mkt_excess + smb + hml + mom, data = .x)
    tidy(model, conf.int = TRUE, conf.level = 0.95) %>%
      filter(term == "(Intercept)") %>%
      select(estimate, std.error, statistic)
  })

# (Double) sorten & Fama-French --------------------------------------------


