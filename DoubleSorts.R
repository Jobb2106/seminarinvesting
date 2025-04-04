# Double sorting, heb ff ander bestandje want werd chaos in mn hoofd


# Data import -------------------------------------------------------------
file_paths <- list.files("data/subset", pattern = "^filtered_\\d{4}-W\\d{2}\\.rds$", full.names = TRUE)



# Code --------------------------------------------------------------------
# df_weekly moet dan RES_week, RSJ_week, permno & next week return minimaal hebben 
rsj_res_portfolios <- df_weekly %>%
  group_by(week_id) %>%
  mutate(
    rsj_bucket = assign_portfolio(pick(everything()), sorting_variable = "RSJ_week", n_portfolios = 5)
  ) %>%
  group_by(week_id, rsj_bucket) %>%
  mutate(
    res_bucket = assign_portfolio(pick(everything()), sorting_variable = "RES_week", n_portfolios = 5)
  ) %>%
  group_by(week_id, rsj_bucket, res_bucket) %>%
  summarise(
    avg_next_ret = mean(next_week_return, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(double_sort_type = "RSJ -> RES")


res_rsj_portfolios <- df_weekly %>%
  group_by(week_id) %>%
  mutate(
    res_bucket = assign_portfolio(pick(everything()), sorting_variable = "RES_week", n_portfolios = 5)
  ) %>%
  group_by(week_id, res_bucket) %>%
  mutate(
    rsj_bucket = assign_portfolio(pick(everything()), sorting_variable = "RSJ_week", n_portfolios = 5)
  ) %>%
  group_by(week_id, res_bucket, rsj_bucket) %>%
  summarise(
    avg_next_ret = mean(next_week_return, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(double_sort_type = "RES -> RSJ")


# Spread ------------------------------------------------------------------
spreads_by_rsj_bucket <- rsj_res_portfolios %>%
  group_by(week_id, rsj_bucket) %>%
  summarise(
    high = max(avg_next_ret, na.rm = TRUE),
    low = min(avg_next_ret, na.rm = TRUE),
    spread = high - low,
    .groups = "drop"
  )

spreads_by_res_bucket <- res_rsj_portfolios %>%
  group_by(week_id, res_bucket) %>%
  summarise(
    high = max(avg_next_ret, na.rm = TRUE),
    low = min(avg_next_ret, na.rm = TRUE),
    spread = high - low,
    .groups = "drop"
  )

# RSJ → RES Spread
model_rsj <- lm(spread ~ 1, data = spreads_by_rsj_bucket)
nw_rsj <- coeftest(model_rsj, vcov = NeweyWest)

# RES → RSJ Spread
model_res <- lm(spread ~ 1, data = spreads_by_res_bucket)
nw_res <- coeftest(model_res, vcov = NeweyWest)


