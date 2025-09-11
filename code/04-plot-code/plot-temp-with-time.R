#### Temp with time ####

# libraries
library(data.table)
library(magrittr)
library(ggplot2)
library(here)

# load data
dat <- data.table::fread(here::here("data", "age_dat_w_dd_base_0_subset.csv"))
str(dat)

dat[, yr := lubridate::year(catch_date)]

# plot
dd_time_plot <- ggplot(data = dat, aes(x = yr, y = dd_scaled, group = yr)) +
  geom_boxplot(alpha = 0.5) +
  geom_hline(aes(yintercept = mean(dd_scaled)),
             linetype = "dashed") +
  xlab("Catch year") +
  ylab("Scaled degree months") +
  scale_x_continuous(breaks = seq(2010, 2023, 2)) +
  scale_y_continuous(breaks = seq(-200, 400, 50)) +
  theme_bw()
dd_time_plot

png(here::here("outputs", "plots", "dm_with_year.png"),
    width = 8, height = 6, units = "in", res = 250)
dd_time_plot
dev.off()
