# This script can be used to calculate the JRNegative and sort portfolios

# Import packages ---------------------------------------------------------
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)


# Import data -------------------------------------------------------------
ajr_files <- list.files("data/metrics/weekly_ajr", pattern = "^ajr_\\d{4}-W\\d{2}\\.rds$", full.names = TRUE)
ajr_by_week <- setNames(ajr_files, str_extract(basename(ajr_files), "\\d{4}-W\\d{2}"))
mu <- sqrt(2 / pi)
types <- c("jump", "continuous") 

# Calculate, sort and test --------------------------------------------------------
for (type in types) {
  cat("Processing type:", type, "\n")
  all_portfolios <- list()

  for (i in 1:(length(ajr_by_week) - 1)) {
    this_week <- names(ajr_by_week)[i]
    next_week <- names(ajr_by_week)[i + 1]
    cat("Processing", this_week, "→", next_week, "\n")
   
    # Load jump-driven stocks
    ajr_df <- readRDS(ajr_by_week[[this_week]]) %>% filter(portfolio == type)
  
    # Load this and next week's data
    df_this <- readRDS(paste0("data/subset/filtered_", this_week, ".rds"))
    df_next <- readRDS(paste0("data/subset/filtered_", next_week, ".rds"))
  
    df_dropped <- tryCatch(
      readRDS(paste0("data/setdifference/dropped_data_", next_week, ".rds")),
      error = function(e) NULL
    )
  
    # Compute JRNegative ----------------------------------------------------
    df_jrn <- df_this %>%
      filter(sym_root %in% ajr_df$sym_root) %>%
      mutate(
        rv_neg = map_dbl(returns_5m, ~ sum((.x[.x < 0])^2)),
        bpv_neg = map_dbl(returns_5m, ~ sum(abs(.x)[-1] * abs(.x)[-length(.x)] * (.x[-1] < 0))),
        jv_neg = pmax(rv_neg - mu^(-2) * bpv_neg, 0),
        jr_neg = jv_neg / rv_neg
      ) %>%
      filter(!is.na(jr_neg), is.finite(jr_neg)) %>%
      group_by(permno, sym_root) %>%
      summarise(
        jr_neg = mean(jr_neg),
        .groups = "drop"
      )
  
    # Compute next week returns ---------------------------------------------
    df_next_week <- df_next %>%
      group_by(permno) %>%
      summarise(returns_week = sum(open_close_log_ret, na.rm = TRUE), .groups = "drop")
  
    df_dropped_returns <- df_dropped %>%
      group_by(permno) %>%
      summarise(returns_week = sum(open_close_log_ret, na.rm = TRUE), .groups = "drop")
  
    df_jrn <- add_next_week_return(df_jrn, df_next_week, df_dropped_returns)
  
    # Assign portfolios -----------------------------------------------------
    df_jrn <- assign_portfolio(df_jrn, jr_neg, 5)
  
    #print(
     # df_jrn %>%
      #  group_by(portfolio) %>%
       # summarise(
        #  avg_jrneg = mean(jr_neg),
         # max_jrneg = max(jr_neg),
          #min_jrneg = min(jr_neg),
          #avg_ret = mean(next_week_return, na.rm = TRUE),
          #.groups = "drop"
        #)
    #)
  
    # Evaluate performance --------------------------------------------------
    portfolios_wide <- df_jrn %>%
      group_by(portfolio) %>%
      summarise(avg_return = mean(next_week_return, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = portfolio, values_from = avg_return, names_prefix = "P") %>%
      mutate(
        num_stocks = nrow(df_jrn),
        week = next_week
      )
  
    all_portfolios[[next_week]] <- portfolios_wide
  }

  # Combine and save results -----------------------------------------------
  final_results <- bind_rows(all_portfolios)
  saveRDS(final_results, paste0("data/metrics/jrn_portfolio_performance_", type, "_combined.rds"))
}
