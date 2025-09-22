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

# AAM comparison -----

# females
dat_f_below <- dat[Sex == "Female" & Age < 14]
dat_f_below_summary <- dat_f_below[, .N, by = "Maturity"]
dat_f_below_summary[, prop := N / sum(N)]
dat_f_below_summary

# males
dat_m_below <- dat[Sex == "Male" & Age < 9]
dat_m_below_summary <- dat_m_below[, .N, by = "Maturity"]
dat_m_below_summary[, prop := N / sum(N)]
dat_m_below_summary
