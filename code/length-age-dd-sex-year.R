### Age + dd + breakpoint stan model

# libraries
library(R2jags)
library(data.table)
library(ggplot2)
library(magrittr)

# load data
dd_dat <- data.table::fread(here::here("data", 'age_dat_w_dd_base0.csv'))
dd_dat <- dd_dat[Sex %in% c("Male", "Female")]

# subset to > 500 dd
dd_dat <- dd_dat[dd <= 500]

# fit with JAGS ----------------------------------------------------------------

# recode sex
dd_dat[, Sex := ifelse(Sex == "Female", 1, 2)]

# get spawning year
dd_dat[, spawn_year := lubridate::year(birth_date)]

# convert to index
dd_dat[, spawn_year_index := as.integer(factor(spawn_year, levels = sort(unique(spawn_year))))]

# list params
params <- c("intercept", "beta_dd", "beta_sex", "beta_age", "v", "tau")
pars <- c("intercept", "beta_dd", "beta_sex[1]", "beta_sex[2]", "beta_age", "tau")
pars_v <- paste0("v[", 1:length(unique(dd_dat$spawn_year)), "]")

mod_dat <- with(dd_dat, list(age = Age,
                             dd = dd,
                             length = Length,
                             sex = Sex,
                             year = spawn_year_index,
                             y = length(unique(spawn_year)),
                             n = nrow(dd_dat)))
str(mod_dat)

# run model
# jags_fit <- R2jags::jags.parallel(model.file = here::here("models",
#                                                  "length-age-dd-sex-year.jags"),
#                          parameters.to.save = c(params, "loglik"),
#                          data = mod_dat,
#                          n.chains = 3,
#                          n.iter = 10000,
#                          n.burnin = 5000,
#                          jags.seed = 1408,
#                          n.thin = 10);beepr::beep(sound = 8)
#
# saveRDS(jags_fit, here::here("outputs", "fits", "length-age-dd-sex-year-jags.Rds"))
jags_fit <- readRDS(here::here("outputs", "fits", "length-age-dd-sex-year-jags.Rds"))

# check output
print(jags_fit)

# extract samples
jags_fit_samples <- coda::as.mcmc(jags_fit)

# traceplot
traceplot_main <- bayesplot::mcmc_trace(jags_fit_samples, pars = pars)
traceplot_main
png(here::here("outputs", "plots", "length-age-dd-sex-year",
               "traceplot_main.png"),
    width = 8, height = 4, units = "in", res = 250)
traceplot_main
dev.off()

traceplot_v <- bayesplot::mcmc_trace(jags_fit_samples, pars = pars_v)
traceplot_v
png(here::here("outputs", "plots", "length-age-dd-sex-year",
               "traceplot_v.png"),
    width = 8, height = 8, units = "in", res = 250)
traceplot_v
dev.off()

# density
density_main <- bayesplot::mcmc_dens_overlay(jags_fit_samples, pars = pars)
density_main
png(here::here("outputs", "plots", "length-age-dd-sex-year",
               "density_main.png"),
    width = 8, height = 4, units = "in", res = 250)
density_main
dev.off()

density_v <- bayesplot::mcmc_dens_overlay(jags_fit_samples, pars = pars_v)
density_v
png(here::here("outputs", "plots", "length-age-dd-sex-year",
               "density_v.png"),
    width = 8, height = 8, units = "in", res = 250)
density_v
dev.off()

# acf
acf_main <- bayesplot::mcmc_acf_bar(jags_fit_samples, pars = pars)
acf_main
png(here::here("outputs", "plots", "length-age-dd-sex-year",
               "acf_main.png"),
    width = 8, height = 6, units = "in", res = 250)
acf_main
dev.off()

acf_v <- bayesplot::mcmc_acf_bar(jags_fit_samples, pars = pars_v)
acf_v
png(here::here("outputs", "plots", "length-age-dd-sex-year",
               "acf_v.png"),
    width = 16, height = 6, units = "in", res = 250)
acf_v
dev.off()

# get loo and waic
# extract loglik samples
loglik_array <- jags_fit$BUGSoutput$sims.list$loglik

# convert to matrix: rows = posterior draws, columns = data points
loglik_matrix <- as.matrix(loglik_array)

# loo and WAIC
loo::waic(loglik_matrix)
loo::loo(loglik_matrix)

# plot model -------------------------------------------------------------------

jags_fit_summary <- summary(jags_fit_samples)
coefs <- jags_fit_summary$statistics %>% as.data.frame()

fit_func <- function(dat, coefs){
  coefs["intercept"] + coefs[paste0("v[", dat$spawn_year_index, "]")] + coefs[paste0("beta_sex[", dat$Sex, "]")] + dat$dd * coefs["beta_dd"] + dat$Age * coefs["beta_age"]
}

# data for prediction
n <- 100
pred_dat <- data.frame(spawn_year_index = rep(1:max(dd_dat$spawn_year_index), length.out = n),
                       Sex = rep(1:2, length.out = n),
                       dd = seq(from = min(dd_dat$dd), to = max(dd_dat$dd), l = n),
                       Age = seq(from = min(dd_dat$Age), to = max(dd_dat$Age), l = n)) %>%
  data.table()
pred_dat <- pred_dat[order(dd), ]

coefs_mean <- coefs$Mean
names(coefs_mean) <- rownames(coefs)
mean_pred <- fit_func(dat = pred_dat, coefs = coefs_mean)

pred_dat <- cbind(pred_dat, mean_pred)

coefs <- jags_fit_summary$quantiles %>% as.data.frame()
coefs_low <- coefs$`2.5%`
names(coefs_low) <- rownames(coefs)
low_pred <- fit_func(dat = pred_dat, coefs = coefs_low)

pred_dat <- cbind(pred_dat, low_pred)

coefs_up <- coefs$`97.5%`
names(coefs_up) <- rownames(coefs)
up_pred <- fit_func(dat = pred_dat, coefs = coefs_up)

pred_dat <- cbind(pred_dat, up_pred)

pred_plot <- ggplot() +
  geom_point(data = dd_dat, aes(x = dd, y = Length), alpha = 0.2, col = "cornflowerblue") +
  geom_line(data = pred_dat, aes(x = dd, y = mean_pred)) +
  geom_ribbon(data = pred_dat, aes(x = dd, ymin = low_pred, ymax = up_pred),
              alpha = 0.2) +
  facet_wrap(. ~spawn_year_index) +
  theme_bw()
pred_plot

png(here::here("outputs", "plots", "length-age-dd-sex-year",
               "pred.png"),
    width = 6, height = 4, units = "in", res = 250)
pred_plot
dev.off()
