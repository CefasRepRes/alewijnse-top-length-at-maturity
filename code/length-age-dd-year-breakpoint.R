###  Length ~ year + sex + dd + age + spawning-year-specific breakpoint

# libraries
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
# jags_fit <- R2jags::jags.parallel(model.file = here::here("models",
#                                                           "length-age-dd-year-breakpoint.jags"),
#                                   parameters.to.save = c(params, "loglik"),
#                                   data = mod_dat,
#                                   n.chains = 3,
#                                   n.iter = 10000,
#                                   n.burnin = 5000,
#                                   jags.seed = 1408,
#                                   n.thin = 10);beepr::beep(sound = 8)

# saveRDS(jags_fit, here::here("outputs", "fits", "length-age-dd-year-breakpoint-jags.Rds"))
jags_fit <- readRDS(here::here("outputs", "fits", "length-age-dd-year-breakpoint-jags.Rds"))

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

# plot model -------------------------------------------------------------------

jags_fit_summary <- summary(jags_fit_samples)
coefs <- jags_fit_summary$statistics %>% as.data.frame()

fit_func <- function(dat, coefs){
    coefs["intercept"] + coefs[paste0("v[", dat$spawn_year_index, "]")] + coefs["beta_dd1"] * (dat$dd - coefs[paste0("delta[", dat$spawn_year_index, "]")]) + coefs["beta_dd2"] * sqrt((dat$dd - coefs[paste0("delta[", dat$spawn_year_index, "]")])^2 + coefs["gamma"]) + dat$Age * coefs["beta_age"]
}

# data for prediction
n <- 100
max_index <- max(dd_dat$spawn_year_index)

pred_dat <- expand.grid(spawn_year_index = 1:max_index, Sex = c(1, 2))
pred_dat <- pred_dat[rep(1:nrow(pred_dat), each = n), ]
pred_dat$dd <- rep(seq(min(dd_dat$dd), max(dd_dat$dd), length.out = n), times = nrow(pred_dat) / n)
pred_dat$Age <- rep(seq(min(dd_dat$Age), max(dd_dat$Age), length.out = n), times = nrow(pred_dat) / n)
pred_dat <- data.table(pred_dat)

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

deltas <- expand.grid(spawn_year_index = 1:max_index, Sex = c(1, 2))
deltas$delta <- coefs_mean[paste0("delta[", deltas$spawn_year_index, "]")]
deltas$delta_up <- coefs_up[paste0("delta[", deltas$spawn_year_index, "]")]
deltas$delta_low <- coefs_low[paste0("delta[", deltas$spawn_year_index, "]")]

pred_plot <- ggplot() +
    geom_point(data = dd_dat, aes(x = dd, y = Length, alpha = 0.2),
               col = "grey40")+
    geom_line(data = pred_dat, aes(x = dd, y = mean_pred)) +
    geom_ribbon(data = pred_dat, aes(x = dd, ymin = low_pred, ymax = up_pred),
                alpha = 0.2) +
    geom_vline(data = deltas, aes(xintercept = delta),
               linetype = "dashed") +
    geom_vline(data = deltas, aes(xintercept = delta_up),
               linetype = "dotted") +
    geom_vline(data = deltas, aes(xintercept = delta_low),
               linetype = "dotted") +
    facet_wrap(.~ spawn_year_index) +
    theme_bw() +
    theme(legend.position = "none")
pred_plot

png(here::here("outputs", "plots", "length-age-dd-year-breakpoint",
               "pred.png"),
    width = 8, height = 8, units = "in", res = 250)
pred_plot
dev.off()
