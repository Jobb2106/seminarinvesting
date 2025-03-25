# Template measures -------------------------------------------------------
# Voorlopig niks mee doen
df <- readRDS("data/clean/returns_5m_all.rds")

df_metrics <- df %>%
  mutate(
    rv = map_dbl(returns_5m, ~ sum((.x)^2, na.rm = TRUE)),
    es_95 = map_dbl(returns_5m, ~ mean(.x[.x < quantile(.x, 0.05, na.rm = TRUE)], na.rm = TRUE))
  ) %>%
  select(sym_root, date, close_crsp, rv, es_95)

saveRDS(df_metrics, "data/metrics/metrics.rds")