library(tidyverse)
library(RSQLite)
library(scales)
library(slider)
library(furrr)
library(tidyfinance)
library(RPostgres)
library(dplyr)      # For data manipulation (mutate, select, etc.)
library(lubridate)  # For floor_date()
library(data.table) # For data.table operations

booktomarket <- bookmarket[, -c(2, 3)]

booktomarket <- booktomarket %>% 
  mutate(date = as.Date(public_date))

setDT(booktomarket)
booktomarket[, month_key := floor_date(date, unit = "month")]
setkey(booktomarket, permno, month_key)

results_book <- merge(
  x = results_beta,
  y = booktomarket[, list(permno, month_key, bm)],
  by = c("permno", "month_key"),
  all.x = TRUE
)


