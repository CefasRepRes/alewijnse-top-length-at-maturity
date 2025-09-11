#### Temp with time ####

# libraries
library(data.table)
library(magrittr)
library(ggplot2)
library(here)

# load data
dat <- data.table::fread(here::here("data", "age_dat_w_dm_base_0_subset.csv"))
str(dat)

dat[, yr := lubridate::year(catch_date)]

# plot
dm_time_plot <- ggplot(data = dat, aes(x = yr, y = dm_scaled, group = yr)) +
  geom_boxplot(alpha = 0.5) +
  geom_hline(aes(yintercept = mean(dm_scaled)),
             linetype = "dashed") +
  xlab("Catch year") +
  ylab("Degree month anomalies") +
  scale_x_continuous(breaks = seq(2010, 2023, 2)) +
  scale_y_continuous(breaks = seq(-200, 400, 50)) +
  theme_bw()
dm_time_plot

png(here::here("outputs", "plots", "dm_with_year.png"),
    width = 8, height = 6, units = "in", res = 250)
dm_time_plot
dev.off()
