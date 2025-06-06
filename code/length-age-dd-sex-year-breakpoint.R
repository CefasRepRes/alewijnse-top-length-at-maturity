### Age + dd + sex and year-specific breakpoint model

# libraries
library(rstan)
library(R2jags)
library(data.table)
library(ggplot2)
library(beepr)

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
params <- c("intercept", "v", "beta_sex", "beta_dd1",
            "beta_dd2", "beta_age", "delta_sex", "delta_year", "gamma", "tau")
pars_main <- c("intercept", "beta_sex[1]", "beta_sex[2]", "beta_dd1",
          "beta_dd2", "beta_age",
          "delta_sex[1]", "delta_sex[2]",
          "gamma", "tau")
pars_v <- paste0("v[", 1:length(unique(dd_dat$spawn_year)), "]")
pars_delta <- paste0("delta_year[", 1:length(unique(dd_dat$spawn_year)), "]")

mod_dat <- with(dd_dat, list(age = Age,
                             length = Length,
                             year = spawn_year_index,
                             dd = dd,
                             sex = Sex,
                             min_dd = min(dd),
                             max_dd = max(dd),
                             y = length(unique(spawn_year)),
                             n = nrow(dd_dat)))
str(mod_dat)

# run model
jags_fit <- R2jags::jags.parallel(model.file = here::here("models",
                                                          "length-age-dd-sex-year-breakpoint.jags"),
                                  parameters.to.save = c(params, "loglik"),
                                  data = mod_dat,
                                  n.chains = 3,
                                  n.iter = 10000,
                                  n.burnin = 5000,
                                  jags.seed = 1408,
                                  n.thin = 10);beepr::beep(sound = 8)

saveRDS(jags_fit, here::here("outputs", "fits", "length-age-dd-sex-year-breakpoint-jags.Rds"))

# check output
print(jags_fit)

# extract samples
jags_fit_samples <- coda::as.mcmc(jags_fit)

# traceplot
traceplot_main <- bayesplot::mcmc_trace(jags_fit_samples, pars = pars_main)
traceplot_main
png(here::here("outputs", "plots", "length-age-dd-sex-year-breakpoint",
               "traceplot_main.png"),
    width = 8, height = 8, units = "in", res = 250)
traceplot_main
dev.off()

traceplot_v <- bayesplot::mcmc_trace(jags_fit_samples, pars = pars_v)
traceplot_v
png(here::here("outputs", "plots", "length-age-dd-sex-year-breakpoint",
               "traceplot_v.png"),
    width = 8, height = 8, units = "in", res = 250)
traceplot_v
dev.off()

traceplot_delta <- bayesplot::mcmc_trace(jags_fit_samples, pars = pars_delta)
traceplot_delta
png(here::here("outputs", "plots", "length-age-dd-sex-year-breakpoint",
               "traceplot_delta.png"),
    width = 8, height = 8, units = "in", res = 250)
traceplot_delta
dev.off()

# density
density_main <- bayesplot::mcmc_dens_overlay(jags_fit_samples, pars = pars_main)
density_main
png(here::here("outputs", "plots", "length-age-dd-sex-year-breakpoint",
               "density_main.png"),
    width = 8, height = 8, units = "in", res = 250)
density_main
dev.off()

density_v <- bayesplot::mcmc_dens_overlay(jags_fit_samples, pars = pars_v)
density_v
png(here::here("outputs", "plots", "length-age-dd-sex-year-breakpoint",
               "density_v.png"),
    width = 8, height = 8, units = "in", res = 250)
density_v
dev.off()

density_delta <- bayesplot::mcmc_dens_overlay(jags_fit_samples, pars = pars_delta)
density_delta
png(here::here("outputs", "plots", "length-age-dd-sex-year-breakpoint",
               "density_delta.png"),
    width = 8, height = 8, units = "in", res = 250)
density_delta
dev.off()

# acf
acf_main <- bayesplot::mcmc_acf_bar(jags_fit_samples, pars = pars_main)
acf_main
png(here::here("outputs", "plots", "length-age-dd-sex-year-breakpoint",
               "acf_main.png"),
    width = 8, height = 8, units = "in", res = 250)
acf_main
dev.off()

acf_v <- bayesplot::mcmc_acf_bar(jags_fit_samples, pars = pars_v)
acf_v
png(here::here("outputs", "plots", "length-age-dd-sex-year-breakpoint",
               "acf_v.png"),
    width = 16, height = 6, units = "in", res = 250)
acf_v
dev.off()

acf_delta <- bayesplot::mcmc_acf_bar(jags_fit_samples, pars = pars_delta)
acf_delta
png(here::here("outputs", "plots", "length-age-dd-sex-year-breakpoint",
               "acf_delta.png"),
    width = 16, height = 6, units = "in", res = 250)
acf_delta
dev.off()

# get loo and waic
# extract loglik samples
loglik_array <- jags_fit$BUGSoutput$sims.list$loglik

# convert to matrix: rows = posterior draws, columns = data points
loglik_matrix <- as.matrix(loglik_array)

# loo and WAIC
loo::waic(loglik_matrix)
loo::loo(loglik_matrix)
