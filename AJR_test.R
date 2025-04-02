# This script can be used to calculate the Adjusted jump-ratio test

# Import packages ---------------------------------------------------------
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)


# Import data -------------------------------------------------------------
file_paths <- list.files("data/subset", pattern = "^filtered_\\d{4}-W\\d{2}\\.rds$", full.names = TRUE)
output_folder <- "data/weekly_filtered_RQ"
output_folder <- "data/weekly_ajr"
if (!dir.exists(output_folder)) dir.create(output_folder)


# Numeric values ----------------------------------------------------------
v = 0.6090 
mu = sqrt(2 / pi)
M = 78 
delta = 1/M 


# Calculate the realized BiPower variation --------------------------------
BPV <- function(returns){
  abs_returns <- abs(returns)
  return(sum(abs_returns[-1] * abs_returns[-length(abs_returns)]))
} 


# Calculate the realized QuadPower variation ------------------------------
QPV <- function(returns){
  abs_returns <- abs(returns)
  return(M * sum(
    abs_returns[1:(length(abs_returns)-3)] *
      abs_returns[2:(length(abs_returns)-2)] *
      abs_returns[3:(length(abs_returns)-1)] *
      abs_returns[4:length(abs_returns)]
  ))
}


# Calculate the adjusted jump-ratio test statistic ------------------------
daily_AJR_stat <- function(returns, QPV, BPV, RV, M, mu) {
  sqrt(M) / sqrt(max(1, QPV / (BPV^2)) * (mu^(-2) * BPV / RV - 1))
}


