library(tidyverse)
library(RSQLite)
library(scales)
library(slider)
library(furrr)
library(tidyfinance)
library(RPostgres)
library(dplyr)
library(lubridate)
library(data.table)

df_ff3 <- tidyfinance::download_data_factors_ff("factors_ff_3_daily", "1993-01-05", "2024-12-31") |> 
  select(date, mkt_excess, risk_free) |>
  collect()

#tidyfinance::set_wrds_credentials(user = "job2506", password = "Daxvax-gihcu1-tecxat")
#crsp_daily <- download_data_crsp("crsp_daily", "2020-12-01", "2020-12-31")

crsp_daily_rf <- daily_crsp |>
  mutate(date = as.Date(date),
  RET = as.numeric(RET)
  )|>
  left_join(df_ff3,
            by = "date"
  ) |>
  mutate(
    permno = PERMNO
  ) |>
  select(permno, RET, risk_free, date)

permnos <- crsp_daily_rf |>
  distinct(permno) |> 
  pull(permno)

#crsp_daily_rf <- crsp_daily_rf |> 
 # filter(date >= as.Date("2020-01-01") & date <= as.Date("2020-6-30"))

batch_size <- 500
batches <- ceiling(length(permnos) / batch_size)

estimate_capm <- function(data, min_obs = 48) {
  data <- data |> drop_na(RET, mkt_excess)
  
  if (nrow(data) < min_obs) {
    return(NA_real_)
  }
  
  tryCatch({
    fit <- lm(RET - risk_free ~ mkt_excess, data = data)
    as.numeric(coef(fit)[2])
  }, error = function(e) {
    return(NA_real_)
  })
}

roll_capm_estimation <- function(data, months, min_obs) {
  data <- data |> arrange(date)
  
  betas <- slide_period_vec(
    .x = data,
    .i = data$date,
    .period = "month",
    .f = ~ estimate_capm(.x, min_obs = min_obs),
    .before = months - 1,
    .complete = FALSE
  )
  
  tibble(date = unique(floor_date(data$date, "month")), beta = betas)
}

beta_daily <- list()

for (j in seq_len(batches)) {
  
  permno_batch <- permnos[((j - 1) * batch_size + 1):min(j * batch_size, length(permnos))]
  
  crsp_daily_sub <- crsp_daily_rf |> 
    filter(permno %in% permno_batch) |> 
    select(permno, date, RET) |> 
    collect()
  
  crsp_daily_sub_nested <- crsp_daily_sub |> 
    inner_join(df_ff3, by = "date") |> 
    mutate(date = floor_date(date, "month")) |> 
    nest(data = c(date, RET, mkt_excess, risk_free)) 
  
  beta_daily[[j]]  <- crsp_daily_sub_nested |> 
    mutate(beta_daily = future_map(
      data, ~ roll_capm_estimation(.x, months = 3, min_obs = 48)
    )) |> 
    unnest(beta_daily) |> 
    select(permno, date, beta_daily = beta) |> 
    drop_na()
  
  message("Batch ", j, " out of ", batches, " done (", percent(j / batches), ")\n")
}

beta_daily <- bind_rows(beta_daily)

saveRDS(beta_daily, "E:/Seminar/Beta/betaresults.rds")

beta_daily <- betaresults




# Toevoegen aan results voor corr matrix ----------------------------------
all_results <- bind_rows(results)

all_results[, month_key := floor_date(date, unit = "month")]
setkey(all_results, permno, month_key)

setDT(beta_daily)

beta_daily[, month_key := floor_date(date, unit = "month")]
setkey(beta_daily, permno, month_key)

results_beta <- merge(
  x = all_results,
  y = beta_daily[, list(permno, month_key, beta_daily)],
  by = c("permno", "month_key"),
  all.x = TRUE
)




