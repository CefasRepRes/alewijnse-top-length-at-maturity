#### Data summary table ####

# libraries
library(data.table)
library(magrittr)
library(here)

# load data
dat <- data.table::fread(here::here("data", "age_dat_w_dm_base_0_subset.csv"))
str(dat)

dat[, yr := lubridate::year(catch_date)]

# summarise
dat_summary_yr <- dat[, .N,
                   by = c("yr", "dataset")]
dat_summary_yr
dat_summary <- dat[, .N,
                   by = c("dataset")]
dat_summary
