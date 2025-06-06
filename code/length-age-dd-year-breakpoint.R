### Age + dd + year-specific breakpoint model

# libraries
library(rstan)
library(R2jags)
library(data.table)
library(ggplot2)
library(beepr)

# load data
dd_dat <- data.table::fread(here::here("data", 'age_dat_w_dd_base0.csv'))
dd_dat <- dd_dat[year %in% c("Male", "Female")]

# subset to > 500 dd
dd_dat <- dd_dat[dd <= 500]

# plot explanatory variables
dd_hist <- ggplot(data = dd_dat, aes(x = dd)) +
  geom_histogram(binwidth = 10) +
  theme_bw()
print(dd_hist)
length_plot <- ggplot(data = dd_dat, mapping = aes(y = Length, x = Age, colour = year)) +
  geom_point(alpha = 0.5) +
  theme(text = element_text(size = 16)) +
  theme_bw()
print(length_plot)
dd_plot <- ggplot(data = dd_dat, mapping = aes(y = Length, x = dd, colour = year)) +
  geom_point(alpha = 0.5) +
  theme(text = element_text(size = 16)) +
  theme_bw()
print(dd_plot)
dd_age_plot <- ggplot(data = dd_dat, mapping = aes(y = Age, x = dd, colour = year)) +
  geom_point(alpha = 0.5) +
  theme(text = element_text(size = 16)) +
  theme_bw()
print(dd_age_plot)

# fit with stan ----------------------------------------------------------------

# # recode year
# dd_dat[, year := ifelse(year == "Female", 0, 1)]
#
# # list coefs
# coefs <- c("alpha", "beta_dd1", "beta_dd2",
#            "beta_age", "beta_year",
#            "delta_0", "delta_year", "gamma", "sigma")
#

# # data
# mod_dat <- with(dd_dat, list(age = Age,
#                              length = Length,
#                              year = year,
#                              dd = dd,
#                              min_dd = min(dd),
#                              max_dd = max(dd),
#                              mean_dd = mean(dd),
#                              sd_dd = sd(dd),
#                              N = nrow(dd_dat)))
#
# # run model
# stan_fit <- rstan::stan(file = here::here("models", "length-age-dd-year-breakpoint.stan"),
#                         model_name = "length_mod",
#                         data = mod_dat,
#                         chains = 3,
#                         iter = 10000,
#                         init = 0,
#                         cores = 3,
#                         seed = 1408,
#                         thin = 10)
#
# # save
# saveRDS(stan_fit,
#         file = here::here("outputs", "fits", "length-age-dd-year-breakpoint-thin.stan"))
#
# # get summary
# summary(stan_fit, pars = coefs)$summary
#
# # traceplot
# traceplot <- bayesplot::mcmc_trace(stan_fit,
#                                    pars = coefs)
# traceplot
# png(here::here("outputs", "plots", "length-age-dd-year-breakpoint",
#                "traceplot.png"),
#     width = 8, height = 8, units = "in", res = 250)
# traceplot
# dev.off()
#
# # density
# density <- bayesplot::mcmc_dens_overlay(stan_fit,
#                                         pars = coefs)
# density
# png(here::here("outputs", "plots", "length-age-dd-year-breakpoint",
#                "density.png"),
#     width = 8, height = 6, units = "in", res = 250)
# density
# dev.off()
#
# # acf
# acf <- bayesplot::mcmc_acf_bar(stan_fit,
#                                pars = coefs)
# acf
# png(here::here("outputs", "plots", "length-age-dd-year-breakpoint",
#                "acf.png"),
#     width = 8, height = 6, units = "in", res = 250)
# acf
# dev.off()
#
# # check LOO and WAIC work
# loo::loo(stan_fit)
# log_lik <- loo::extract_log_lik(stan_fit)
# loo::waic(log_lik)

# fit with JAGS ----------------------------------------------------------------

# get spawning year
dd_dat[, spawn_year := lubridate::year(birth_date)]

# convert to index
dd_dat[, spawn_year_index := as.integer(factor(spawn_year, levels = sort(unique(spawn_year))))]

# list params
params <- c("intercept", "v", "beta_dd1",
            "beta_dd2", "beta_age", "delta", "gamma", "tau")
pars_main <- c("intercept", "beta_dd1",
          "beta_dd2", "beta_age",
          "gamma", "tau")
pars_v <- paste0("v[", 1:length(unique(dd_dat$spawn_year)), "]")
pars_delta <- paste0("delta[", 1:length(unique(dd_dat$spawn_year)), "]")

mod_dat <- with(dd_dat, list(age = Age,
                             length = Length,
                             year = spawn_year_index,
                             dd = dd,
                             min_dd = min(dd),
                             max_dd = max(dd),
                             y = length(unique(spawn_year)),
                             n = nrow(dd_dat)))
str(mod_dat)

# run model
jags_fit <- R2jags::jags.parallel(model.file = here::here("models",
                                                          "length-age-dd-year-breakpoint.jags"),
                                  parameters.to.save = c(params, "loglik"),
                                  data = mod_dat,
                                  n.chains = 3,
                                  n.iter = 10000,
                                  n.burnin = 5000,
                                  jags.seed = 1408,
                                  n.thin = 10);beepr::beep(sound = 8)

saveRDS(jags_fit, here::here("outputs", "fits", "length-age-dd-year-breakpoint-jags.Rds"))

# check output
print(jags_fit)

# extract samples
jags_fit_samples <- coda::as.mcmc(jags_fit)

# traceplot
traceplot_main <- bayesplot::mcmc_trace(jags_fit_samples, pars = pars_main)
traceplot_main
png(here::here("outputs", "plots", "length-age-dd-year-breakpoint",
               "traceplot_main.png"),
    width = 8, height = 8, units = "in", res = 250)
traceplot_main
dev.off()

traceplot_v <- bayesplot::mcmc_trace(jags_fit_samples, pars = pars_v)
traceplot_v
png(here::here("outputs", "plots", "length-age-dd-year-breakpoint",
               "traceplot_v.png"),
    width = 8, height = 8, units = "in", res = 250)
traceplot_v
dev.off()

traceplot_delta <- bayesplot::mcmc_trace(jags_fit_samples, pars = pars_delta)
traceplot_delta
png(here::here("outputs", "plots", "length-age-dd-year-breakpoint",
               "traceplot_delta.png"),
    width = 8, height = 8, units = "in", res = 250)
traceplot_delta
dev.off()

# density
density_main <- bayesplot::mcmc_dens_overlay(jags_fit_samples, pars = pars_main)
density_main
png(here::here("outputs", "plots", "length-age-dd-year-breakpoint",
               "density_main.png"),
    width = 8, height = 8, units = "in", res = 250)
density_main
dev.off()

density_v <- bayesplot::mcmc_dens_overlay(jags_fit_samples, pars = pars_v)
density_v
png(here::here("outputs", "plots", "length-age-dd-year-breakpoint",
               "density_v.png"),
    width = 8, height = 8, units = "in", res = 250)
density_v
dev.off()

density_delta <- bayesplot::mcmc_dens_overlay(jags_fit_samples, pars = pars_delta)
density_delta
png(here::here("outputs", "plots", "length-age-dd-year-breakpoint",
               "density_delta.png"),
    width = 8, height = 8, units = "in", res = 250)
density_delta
dev.off()

# acf
acf_main <- bayesplot::mcmc_acf_bar(jags_fit_samples, pars = pars_main)
acf_main
png(here::here("outputs", "plots", "length-age-dd-year-breakpoint",
               "acf_main.png"),
    width = 8, height = 8, units = "in", res = 250)
acf_main
dev.off()

acf_v <- bayesplot::mcmc_acf_bar(jags_fit_samples, pars = pars_v)
acf_v
png(here::here("outputs", "plots", "length-age-dd-year-breakpoint",
               "acf_v.png"),
    width = 16, height = 6, units = "in", res = 250)
acf_v
dev.off()

acf_delta <- bayesplot::mcmc_acf_bar(jags_fit_samples, pars = pars_delta)
acf_delta
png(here::here("outputs", "plots", "length-age-dd-year-breakpoint",
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
