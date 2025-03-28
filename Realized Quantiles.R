# This script serves as realized quantiles code


# Data --------------------------------------------------------------------
# Laad de data uit GitHub
df <- readRDS("data/clean/returns_5m_all_subset.rds")


# Realized Quantiles ------------------------------------------------------
p <- 0.05
c <- 78
H <- 0.5
scaling_factor <- c^H

# Step 1: Group by day and compute quantile and ES
results <- df %>%
  group_by(date) %>%
  summarise(
    Q_p = quantile(return, probs = p, na.rm = TRUE),
    ES_p = mean(return[return <= quantile(return, probs = p, na.rm = TRUE)], na.rm = TRUE)
  ) %>%
  mutate(
    RQ = scaling_factor * Q_p,
    RES = scaling_factor * ES_p
  )

# Hierna dan (double) sorten & opnieuw Fama-French regressie