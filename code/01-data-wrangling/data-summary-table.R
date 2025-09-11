#### Data summary table ####

# libraries
library(data.table)
library(magrittr)
library(ggplot2)
library(here)

# load data
dat <- data.table::fread(here::here("data", "age_dat_w_dd_base_0_subset.csv"))
str(dat)

dat[, yr := lubridate::year(catch_date)]

# summarise
dat_summary <- dat[, .N,
                   by = c("yr", "dataset")]
dat_summary <- dat[, .N,
                   by = c("dataset")]
